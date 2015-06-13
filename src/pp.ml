open Consts
open Format
open Lang

(***** Helpers {{{ *****)

let fpf         = fprintf ;;
let ident ppf s = fpf ppf "%s" s ;;
let kwd   ppf s = fpf ppf "%s" s ;;

(***** }}} *****)

(***** Types {{{ *****)

let prec_of_typ (t:typ) =
  match t with
  | TBase _ -> 100
  | TArr  _ -> 50

let rec fpf_typ ppf ((lvl, t): int * typ) =
  let this_lvl = prec_of_typ t in
  (if this_lvl < lvl then fpf ppf "(");
  begin match t with
  | TBase x       -> fpf ppf "%a" ident x
  | TArr (t1, t2) -> fpf ppf "@[<2>%a ->@ %a@]" fpf_typ (this_lvl+1, t1) fpf_typ (this_lvl, t2)
  end;
  (if this_lvl < lvl then fpf ppf ")")

let rec fpf_typ_list ppf (ts: typ list) =
  match ts with
  | []    -> ()
  | [t]   -> fpf ppf "%a" fpf_typ (0, t)
  | t::ts -> fpf ppf "%a * %a" fpf_typ (0, t) fpf_typ_list ts

(***** }}} *****)

(***** Declarations and expressions {{{ *****)

let prec_of_exp (e:exp) : int =
  match e with
  | EApp    _      -> 500
  | EFun _ | EFix _ | EPFun _ | ELet _ -> 100
  | ECtor  (_, es) -> if List.length es = 0 then 1000 else 500
  | EMatch  _      -> 200
  | _              -> 1000

let prec_of_value (v:value) : int =
  match v with
  | VFun _ | VPFun _ -> 100
  | VCtor (_, vs) -> if List.length vs = 0 then 1000 else 500

let rec is_nat_literal (e:exp) : bool =
  match e with
  | ECtor ("O", [])  -> true
  | ECtor ("S", [e]) -> is_nat_literal e
  | _                -> false

let rec is_list_literal (e:exp) : bool =
  match e with
  | ECtor ("Nil", [])      -> true
  | ECtor ("Cons", [_; e]) -> is_list_literal e
  | _                      -> false

let fpf_nat_literal ppf e =
  let rec count n e =
    match e with
    | ECtor ("O", [])  -> fpf ppf "%d" n
    | ECtor ("S", [e]) -> count (n+1) e
    | _ -> raise @@ Internal_error "(fpf_nat_literal) non-nat literal encountered"
  in
  count 0 e

let rec fpf_id_list ppf (xs:id list) =
  match xs with
  | []    -> ()
  | [x]   -> fpf ppf "%a" ident x
  | x::xs -> fpf ppf "%a,@ %a" ident x fpf_id_list xs

let rec fpf_pat ppf (p:pat) =
  match p with
  | PCtor (c, xs) ->
      fpf ppf "@[<2>%a" ident c;
      if List.length xs <> 0 then
        fpf ppf " (%a)" fpf_id_list xs
      else
        ();
      fpf ppf "@]"

let fpf_ctor ppf ((c, ts): ctor) =
  fpf ppf "%a of %a" ident c fpf_typ_list ts

let rec fpf_ctors ppf (cs:ctor list) =
  match cs with
  | []    -> ()
  | [c]   -> fpf ppf "| %a" fpf_ctor c
  | c::cs -> fpf ppf "| %a@\n%a" fpf_ctor c fpf_ctors cs

let rec fpf_branch ppf ((lvl, b):int * branch) =
  let (p, e) = b in
  fpf ppf "@[<2>| %a -> %a@]" fpf_pat p fpf_exp (lvl, e)

and fpf_branches ppf ((lvl, bs):int * branch list) =
  match bs with
  | [] -> ()
  | [b] -> fpf ppf "%a" fpf_branch (lvl, b)
  | b::bs -> fpf ppf "%a@\n%a" fpf_branch (lvl, b) fpf_branches (lvl, bs)

and fpf_env_one ppf ((x, v):id * value) =
  fpf ppf "%a := %a" ident x fpf_value (0, v)

and fpf_env ppf (ev:env) =
  match ev with
  | []    -> fpf ppf "·"
  | [v]   -> fpf ppf "%a" fpf_env_one v
  | v::vs -> fpf ppf "%a,@\n%a" fpf_env_one v fpf_env vs

and fpf_evidence_list ppf (vs:evidence list) =
  match vs with
  | []    -> ()
  | [v]   -> fpf ppf "%a" fpf_evidence v
  | v::vs -> fpf ppf "%a;@  %a" fpf_evidence v fpf_evidence_list vs

and fpf_evidence ppf ((env, v):evidence) =
  fpf ppf "%a |-> %a" fpf_env env fpf_value (0, v)

and fpf_exp_list ppf (es:exp list) =
  match es with
  | []    -> ()
  | [e]   -> fpf ppf "%a" fpf_exp (0, e)
  | e::es -> fpf ppf "%a,@ %a" fpf_exp (0, e) fpf_exp_list es

and fpf_arg ppf ((x, t):arg) = fpf ppf "(%a:%a)" ident x fpf_typ (0, t)

and fpf_arg_list ppf (xs:arg list) =
  match xs with
  | []    -> ()
  | [x]   -> fpf ppf "%a" fpf_arg x
  | x::xs -> fpf ppf "%a %a" fpf_arg x fpf_arg_list xs

and fpf_ios ppf ((lvl, ios):int * (exp * exp) list) =
  match ios with
  | []            -> ()
  | [(e1, e2)]    -> fpf ppf "%a => %a" fpf_exp (lvl, e1) fpf_exp (lvl, e2)
  | (e1, e2)::ios -> fpf ppf "%a => %a@\n| %a" fpf_exp (lvl, e1) fpf_exp (lvl, e2) fpf_ios (lvl, ios)

and fpf_decl ppf (d:decl) =
  match d with
  | DData (dt, cs) -> fpf ppf "@[<2>type %a =@\n%a@]" ident dt fpf_ctors cs
  | DLet (x, is_rec, xs, t, e) ->
    fpf ppf "@[<2>let ";
    (if is_rec then fpf ppf "rec ");
    if List.length xs = 0 then
      fpf ppf "%a : %a =@\n%a@]@\n;;" ident x fpf_typ (0, t) fpf_exp (0, e)
    else
      fpf ppf "%a %a : %a =@\n%a@]@\n;;"
        ident x fpf_arg_list xs fpf_typ (0, t)
        fpf_exp (0, e);

and fpf_exp ppf ((lvl, e):int * exp) =
  if !pretty_ctors && is_nat_literal e then
    fpf_nat_literal ppf e
  else if !pretty_ctors && is_list_literal e then
    fpf_list_literal ppf e
  else
    let this_lvl = prec_of_exp e in
    (if this_lvl < lvl then fpf ppf "(");
    begin match e with
    | EVar x -> fpf ppf "%a" ident x
    | EApp (e1, e2) -> fpf ppf "@[<2>%a@ %a@]" fpf_exp (this_lvl, e1) fpf_exp (this_lvl + 1, e2)
    | EFun (x, e) -> fpf ppf "@[<2>fun %a ->@ %a@]" fpf_arg x fpf_exp (this_lvl, e)
    | ELet (f, is_rec, xs, t, e1, e2) ->
        fpf ppf "@[<2>let ";
        (if is_rec then fpf ppf "rec ");
        fpf ppf "%a %a : %a =@\n%a@]@\n"
          ident f fpf_arg_list xs fpf_typ (0, t)
          fpf_exp (this_lvl, e1);
        fpf ppf "@[<2>in@\n%a@]" fpf_exp (this_lvl, e2)
    | ECtor (c, es)  ->
        if List.length es = 0 then
          fpf ppf "@[<2>%a@]" ident c
        else
          fpf ppf "@[<2>%a (%a)@]" ident c fpf_exp_list es
    | EMatch (e, bs) -> fpf ppf "@[<2>match %a with@\n%a@]" fpf_exp (0, e) fpf_branches (this_lvl+1, bs)
    | EPFun ios -> fpf ppf "@[<2>%a@]" fpf_ios (this_lvl, ios)
    | EFix (f, x, t, e) -> fpf ppf "@[<2>fix %a %a : %a =@ %a@]"
      ident f fpf_arg x fpf_typ (0, t) fpf_exp (this_lvl, e)
    end;
    (if this_lvl < lvl then fpf ppf ")")

and fpf_list_literal ppf e =
  let rec fpf_elems ppf e =
    match e with
    | ECtor ("Nil", []) -> ()
    | ECtor ("Cons", [e; ECtor ("Nil", [])]) -> fpf ppf "%a" fpf_exp (0, e)
    | ECtor ("Cons", [e1; e2]) -> begin
      fpf ppf "%a; " fpf_exp (0, e1);
      fpf_elems ppf e2
    end
    | _ -> raise @@ Internal_error "(fpf_list_literal) non-list literal encountered"
  in
  fpf ppf "[";
  fpf_elems ppf e;
  fpf ppf "]"

and fpf_value_list ppf (vs:value list) =
  match vs with
  | []    -> ()
  | [v]   -> fpf ppf "%a" fpf_value (0, v)
  | v::vs -> fpf ppf "%a,@ %a" fpf_value (0, v) fpf_value_list vs

and fpf_value_pairs ppf ((lvl, vps):int * (value * value) list) =
  match vps with
  | []            -> ()
  | [(v1, v2)]    -> fpf ppf "%a => %a" fpf_value (lvl, v1) fpf_value (lvl, v2)
  | (v1, v2) :: vps -> fpf ppf "%a => %a@\n| %a" fpf_value (lvl, v1) fpf_value (lvl, v2) fpf_value_pairs (lvl, vps)

and fpf_value ppf ((lvl, v):int * value) =
  let this_lvl = prec_of_value v in
  (if this_lvl < lvl then fpf ppf "(");
  begin match v with
  | VCtor (c, vs) ->
      if List.length vs = 0 then
        fpf ppf "@[<2>%a@]" ident c
      else
        fpf ppf "@[<2>%a (%a)@]" ident c fpf_value_list vs
  | VFun (x, e, _) ->
      fpf ppf "@[<2>fun %a ->@ %a@]" ident x fpf_exp (this_lvl, e)
  | VPFun vps -> fpf ppf "@[<2>%a@]" fpf_value_pairs (this_lvl, vps)
  end;
  (if this_lvl < lvl then fpf ppf ")")

let rec fpf_decls ppf (ds:decl list) =
  match ds with
  | []    -> ()
  | [d]   -> fpf ppf "%a" fpf_decl d
  | d::ds -> fpf ppf "%a@\n@\n%a" fpf_decl d fpf_decls ds

let rec fpf_synth_problem ppf ((x, t, es):synth_problem) =
  fpf ppf "@[<2>let %a : %a |>@ { %a } =@ ?@]"
    ident x fpf_typ (0, t) fpf_exp_list es

let rec fpf_prog ppf ((ds, p):prog) =
  if List.length ds > 0 then
    fpf ppf "%a@\n@\n%a" fpf_decls ds fpf_synth_problem p
  else
    fpf ppf "%a" fpf_synth_problem p

(***** }}} *****)

(***** Contexts {{{ *****)

(* TODO: print out stuff for bindspecs... *)
let fpf_bindspec ppf (b:Ctx.bindspec) =
  match b with
  | Ctx.BRec f -> fpf ppf "r(%a)" ident f
  | Ctx.BArg x -> fpf ppf "!(%a)" ident x
  | Ctx.BDec x -> fpf ppf "v(%a)" ident x

let rec fpf_bindspec_list ppf (bs:Ctx.bindspec list) =
  match bs with
  | [] -> ()
  | [b] -> fpf ppf "%a" fpf_bindspec b
  | b :: bs -> fpf ppf "%a, %a" fpf_bindspec b fpf_bindspec_list bs

let fpf_binding ppf ((x, (t, bs)):id * (typ * Ctx.bindspec list)) =
  fpf ppf "%a:%a [%a]" ident x fpf_typ (0, t) fpf_bindspec_list bs

let rec fpf_ctx ppf (g:Ctx.t) =
  match g with
  | [] -> ()
  | [bind] -> fpf ppf "%a" fpf_binding bind
  | bind :: g -> fpf ppf "%a, %a" fpf_binding bind fpf_ctx g

(***** }}} *****)

let pp_aux (f:formatter -> 'a -> unit) : 'a -> string =
  (fun (x:'a) -> f str_formatter x; flush_str_formatter ())

let pp_typ t = (fpf_typ str_formatter (0, t); flush_str_formatter ()) ;;
let pp_exp e = (fpf_exp str_formatter (0, e); flush_str_formatter ()) ;;
let pp_value v = (fpf_value str_formatter (0, v); flush_str_formatter ()) ;;
let pp_decl = pp_aux fpf_decl    ;;
let pp_pat  = pp_aux fpf_pat     ;;
let pp_prog = pp_aux fpf_prog    ;;
let pp_ctx  = pp_aux fpf_ctx     ;;
let pp_env  = pp_aux fpf_env     ;;
let pp_evidence = pp_aux fpf_evidence ;;
let pp_evidence_list = pp_aux fpf_evidence_list ;;
