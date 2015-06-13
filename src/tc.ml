open Lang
open Pp

exception Type_error of string

(***** Helpers {{{ *****)

let rec match_branches (s:Sig.t) (bs:branch list) : (Sig.args_t * branch) list =
  match bs with
  | [] ->
    if List.length s > 0 then
      raise @@ Type_error "Missing branches in pattern match"
    else
      []
  | (p, e)::bs ->
    begin match p with
      | PCtor (c, xs) ->
        let (s, ctor) = Sig.extract_ctor c s in
        (ctor, (p, e))::(match_branches s bs)
    end

let rec all_eq (l:'a list) : 'a =
  match l with
  | [] -> raise @@ Internal_error "Expected non-empty type list"
  | [x] -> x
  | x1::x2::l ->
    if x1 <> x2 then
      raise @@ Type_error "Type mismatch in branches"
    else
      all_eq (x2::l)

(***** }}} *****)

(***** Well-founded recursion {{{ *****)

let rec is_valid_app (g:Ctx.t) (e1:exp) (e2:exp) : bool =
  let check_recursive_fun g e =
    match e with
    | EVar x -> if Ctx.is_rec_fun x g then Some x else None
    | _ -> None
  in
  let is_decreasing_arg g f e =
    match e with
    | EVar x -> Ctx.is_dec_arg x f g
    | _ -> false
  in
  match check_recursive_fun g e1 with
  | None -> true
  | Some f -> is_decreasing_arg g f e2

let rec check_recursive_arg_match (g:Ctx.t) (e:exp) : id option =
  match e with
  | EVar x -> Ctx.fetch_dec_fn x g
  | _ -> None

(***** }}} *****)

(***** Typechecking {{{ *****)

let rec tc_exp (s:Sig.t) (g:Ctx.t) (e:exp) : typ =
  match e with
  | EVar x ->
    begin match Ctx.lookup x g with
    | Some t -> t
    | None -> raise @@ Type_error (x ^ " not declared")
    end
  | EApp (e1, e2) ->
    begin match tc_exp s g e1 with
    | TArr (t1, t2) ->
      let t1' = tc_exp s g e2 in
      if t1 <> t1' then
        raise @@ Type_error ("Type mismatch in application " ^ pp_exp e ^ ". " ^ pp_typ t1' ^ " found, but " ^ pp_typ t1 ^ " expected.")
      else if not @@ is_valid_app g e1 e2 then
        raise @@ Type_error (Printf.sprintf "Non-decreasing argument wrt recursive function: %s" (Pp.pp_exp e))
      else
        t2
    | _ -> raise @@ Type_error "Function type not found"
    end
  | EFun ((x, t1), e) ->
    let g   = Ctx.append [(x, (t1, []))] g in
    let t2  = tc_exp s g e in
    TArr (t1, t2)
  | ELet (f, is_rec, xs, t, e1, e2) ->
    if List.length xs = 0 then
      (* Value binding *)
      let t = tc_exp s g e1 in
      tc_exp s (Ctx.append [(f, (t, []))] g) e2
    else
      (* Function binding *)
      let var_spec = if is_rec then [Ctx.BArg f] else [] in
      let g'   = Ctx.append (List.map (fun (x, t) -> (x, (t, var_spec))) xs) g in
      let tarr = (List.map snd xs) @ [t] |> types_to_arr in
      let g' = if is_rec then
        Ctx.append [(f, (tarr, List.map (fun x -> Ctx.BRec x) (List.map fst xs)))] g'
      else
        g'
      in
        let t' = tc_exp s g' e1 in
        if t <> t' then
          raise @@ Type_error "Type mismatch in function body"
        else
          tc_exp s (Ctx.append [(f, (tarr, []))] g) e2
  | ECtor (c, es) ->
    begin match Sig.lookup_ctor c s with
    | Some (ts, d) ->
      if List.length ts <> List.length es then
        raise @@ Type_error (Printf.sprintf "Incorrect number of arguments to constructor %s" c)
      else
        List.iter (fun (t, e) ->
          let t' = tc_exp s g e in
          if t <> t' then
            raise @@ Type_error (Printf.sprintf "Type mismatch in constructor: expected %s but found %s : %s" (Pp.pp_typ t) (Pp.pp_exp e) (Pp.pp_typ t'))
          else
            ()) (List.combine ts es) ; TBase d
    | None ->
      raise @@ Type_error (Printf.sprintf "Constructor not found: %s" c)
    end
  | EMatch (e, bs) ->
    begin match tc_exp s g e with
    | TBase d ->
      let fn_opt = check_recursive_arg_match g e in
      let ctors = Sig.restrict d s in
      (* BUG: need to typecheck patterns in branches... *)
      let ts = tc_branches s g ctors fn_opt bs in all_eq ts
    | _ -> raise @@ Type_error "Datatype expected in match"
    end
  | EPFun ios ->
      let ts = List.map (fun (e1, e2) ->
        let t1 = tc_exp s g e1 in
        let t2 = tc_exp s g e2 in
        TArr (t1, t2)) ios
      in
      if List.length ios = 0 then
        raise @@ Type_error "Partial function has no cases"
      else if not (Util.all_eq (=) ts) then
        raise @@ Type_error "Partial function case types do not match"
      else
        List.hd ts
  | EFix (f, (x, t1), t2, e) ->
      let g = Ctx.append [(f, (TArr (t1, t2), [Ctx.BRec x])); (x, (t1, [Ctx.BArg f]))] g in
      let t2' = tc_exp s g e in
      if t2 <> t2' then
        raise @@ Type_error ("Type mismatch in function body " ^ pp_exp e ^ ". " ^ pp_typ t2' ^ " found, but " ^ pp_typ t2 ^ " expected.")
      else
        TArr (t1, t2)

and tc_evidence (s:Sig.t) (g:Ctx.t) (t:typ) (es:exp list) : unit =
  List.iter (fun e -> if t <> tc_exp s g e then raise @@ Type_error "Type mismatch in evidence") es

and tc_branches (s:Sig.t) (g:Ctx.t) (ctors:Sig.t) (fn_opt:id option) (bs:branch list) : typ list =
  let sbs = match_branches ctors bs in
  List.map (fun (ctor, b) -> tc_branch s g ctor fn_opt b) sbs

and tc_branch (s:Sig.t) (g:Ctx.t) (ctor:Sig.args_t) (fn_opt:id option) ((p, e):branch) : typ =
  let annotate_dec fn_opt g =
    match fn_opt with
    | Some f -> Ctx.mark_as_decreasing f g
    | None -> g
  in
  let g'  = Ctx.gather_binders s p |> annotate_dec fn_opt in
  tc_exp s (List.append g' g) e

let rec tc_decl (s:Sig.t) (g:Ctx.t) (d:decl) : Sig.t * Ctx.t =
  match d with
  | DData (dt, cs) -> (Sig.append s (Sig.make_sig_from_data dt cs), g)
  | DLet (f, is_rec, xs, t, e) ->
    if List.length xs = 0 then
      (* Value binding *)
      let t' = tc_exp s g e in
      if t = t' then
        (s, Ctx.insert f t g)
      else
        raise @@ Type_error "Type mismatch with let"
    else
      (* Function binding *)
      let var_spec = if is_rec then [Ctx.BArg f] else [] in
      let g'   = Ctx.append (List.map (fun (x, t) -> (x, (t, var_spec))) xs) g in
      let tarr = (List.map snd xs) @ [t] |> types_to_arr in
      let g' = if is_rec then
        Ctx.append [(f, (tarr, List.map (fun x -> Ctx.BRec x) (List.map fst xs)))] g'
      else
        g'
      in
        let t' = tc_exp s g' e in
        if t <> t' then
          raise @@ Type_error "Type mismatch with let"
        else
          (s, Ctx.insert f tarr g)

let rec tc_decls (s:Sig.t) (g:Ctx.t) (ds:decl list) : Sig.t * Ctx.t =
  List.fold_left (fun (s, g) d -> tc_decl s g d) (s, g) ds

let rec tc_synth_problem (s:Sig.t) (g:Ctx.t) ((x, t, vs):synth_problem) =
  tc_evidence s g t vs

let rec tc_prog ((ds, p):prog) : Sig.t * Ctx.t =
  let (s, g) = tc_decls Sig.empty Ctx.empty ds in
  let _ = tc_synth_problem s g p in
  (s, g)

(***** }}} *****)
