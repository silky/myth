open Core.Std
open Consts
open Lang

type rtree =
  { t                 : typ
  ; mutable sz        : int
  ; mutable lambdas   : int
  ; g                 : Ctx.t
  ; vs                : evidence list
  ; mutable timed_out : bool
  ; mutable es        : exp list
  ; alts              : rnode list
  ; mutable matches   : rtree_matches
  }

and rnode =
  | SAbs   of id * arg * typ * rtree
  | SCtor  of id * rtree list

and rtree_matches = (int * rmatch list) option
and rmatch  = exp * rbranch list
and rbranch = pat * rtree

let rec rtree_size (t:rtree) : int =
  let rnode_size n =
    match n with
    | SAbs (_, _, _, t) -> rtree_size t
    | SCtor (_, ts) -> List.fold_left ~f:(fun acc t -> rtree_size t + acc) ~init:0 ts
  in
  let rmatch_size (m:rmatch) : int =
    let (_, bs) = m in
    List.fold_left ~f:(fun acc (_, t) -> rtree_size t + acc) ~init:0 bs
  in
  let matches_size (mopt:rtree_matches) : int =
    match mopt with
    | None -> 0
    | Some (_, ms) -> List.fold_left ~f:(fun acc m -> rmatch_size m + acc) ~init:0 ms
  in
  List.fold_left ~f:(fun acc n -> acc + rnode_size n) ~init:(1 + matches_size t.matches) t.alts

(***** Pretty printing {{{ *****)

type pretty_line = int * string

let print_whitespace (n:int) = (String.make (n + 1) '*') ^ "   "
let pretty_lines (lines:pretty_line list) : string list =
  List.map ~f:(fun (n, s) -> print_whitespace n ^ s) lines

let rec stringify (ss:string list) : string =
  match ss with
  | [] -> ""
  | [s] -> s
  | s :: ss -> s ^ "\n" ^ stringify ss

let stringify_rtree_matches (rm:rtree_matches) : string =
  match rm with
  | None -> "growable"
  | Some (k, ms) -> Printf.sprintf "scrutinee_size = %d, #/matches = %d" k (List.length ms)

let rec build_lines_rtree (k:int) (t:rtree) : pretty_line list =
  let altlines   = List.concat_map ~f:(build_lines_rnode k t.t) t.alts in
  let matchlines = build_lines_matches k t.t t.matches in
  let lines      = altlines @ matchlines in
  (k, Printf.sprintf "* :: %s [E-size = %d, timed_out = %b, exp_count = %d, %s]"
    (Pp.pp_typ t.t) t.sz t.timed_out (List.length t.es) (stringify_rtree_matches t.matches)) :: lines

and build_lines_matches (k:int) (tt:typ) (mopt:rtree_matches) : pretty_line list =
  match mopt with
  | None -> []
  | Some (_, ms) -> List.concat_map ~f:(build_lines_rmatch k tt) ms

and build_lines_rmatch (k:int) (tt:typ) (m:rmatch) : pretty_line list =
  let (e, bs) = m in
  let s = Printf.sprintf "match %s :: %s" (Pp.pp_exp e) (Pp.pp_typ tt) in
  (k, s) :: List.concat_map ~f:(fun (p, t) ->
    let s = Printf.sprintf "| %s ->" (Pp.pp_pat p) in
    (k+1, s) :: build_lines_rtree (k+2) t) bs

and build_lines_rnode (k:int) (tt:typ) (n:rnode) : pretty_line list =
  match n with
  | SAbs (f, (x, t1), t2, t) ->
      let s = Printf.sprintf "fix %s (%s:%s) : %s :: %s" f x (Pp.pp_typ t1) (Pp.pp_typ t2) (Pp.pp_typ tt) in
      (k, s) :: build_lines_rtree (k+1) t
  | SCtor (c, ts) ->
      let s = Printf.sprintf "ctor %s :: %s" c (Pp.pp_typ tt) in
      (k, s) :: List.concat_map ~f:(build_lines_rtree (k+1)) ts

let pp_rtree t = build_lines_rtree 0 t |> pretty_lines |> stringify

(***** }}} *****)

(***** Refinement tree construction {{{ *****)

type ctor_val      = id * value list * env * value
type synth_branch  = id * pat * Ctx.t * evidence list
type eval_val      = { value:value; env:env; goal:value }
type scrutinee_val = { e:exp; vs:eval_val list }

(***** Split graph construction helpers {{{ *****)

(* Splits the evidence as input-output (function) examples *)
let split_io_evidence (f:id) (x:id) (env:env) (vs:evidence list) : evidence list =
  List.map ~f:begin fun (env', v) ->
    let fbind = (f, v) in
    match v with
    | VPFun ios -> List.map ~f:(fun (v1, v2) ->
        let xbind = (x, v1) in
        (fbind :: xbind :: env', v2)) ios
    | _ -> failwith "(split_io_evidence) non-IO evidence in goal position"
  end vs
    |> List.concat

(* Splits the evidence as (same-head) constructor examples *)
let split_ctor_evidence (vs:evidence list) : evidence list list =
  let len = List.hd_exn vs
    |> function
      | (_, VCtor (_, vs)) -> List.length vs
      | _ -> failwith "(split_ctor_evidence) constructor expected"
  in
  (* env1 : C(e11, .., en1) .. envk : C(e1k, .., enk) becomes
   * [(env, e11) .. (envk, e1k)] .. [(env, en1) .. (envk, enk)] *)
  List.map ~f:(fun n ->
    List.fold_right ~f:(fun (env', v) acc ->
      match v with
      | VCtor (_, vs) -> (env', List.nth_exn vs n) :: acc
      | _ -> failwith "(split_ctor_evidence) constructor expected")
      ~init:[] vs)
    (Util.range len)

(* Constructs a pattern with fresh names for the given constructor *)
let make_pattern (s:Sig.t) (g:Ctx.t) (c:id) : pat =
  match Sig.lookup_ctor c s with
  | Some (ts, _) ->
    PCtor (c, List.fold_left
      ~f:(fun xs t -> (gen_var_base t
        |> fresh_id_from_list (xs @ Ctx.ids g)) :: xs)
      ~init:[] ts |> List.rev)
  | None -> failwith "make_pattern: constructor not found"

let rec evaluate_to_values (s:Sig.t) (initenv:env) (vs:evidence list) (e:exp) : scrutinee_val =
  let vs = List.fold_right ~f:(fun (env, v) acc ->
    try
      { value = Eval.eval (env @ initenv) e; env = env; goal = v } :: acc
    with
      Eval.Eval_error _ -> acc) ~init:[] vs
  in
    { e = e; vs = vs }

(* Evaluates the given expression to a constructor value *)
let rec evaluate_to_ctors (sv:scrutinee_val) : ctor_val list =
  List.map ~f:(fun ev ->
    match ev.value with
    | VCtor (c, vs) -> (c, vs, ev.env, ev.goal)
    | _ -> failwith "(evaluate_to_ctors) non-constructor encountered") sv.vs

(* Extracts the datatype name of a particular constructor value *)
let datatype_of_ctor_val (s:Sig.t) ((c, _, _, _):ctor_val) : id =
  match Sig.lookup_ctor c s with
  | Some (_, dt) -> dt
  | None -> failwith "(datatype_of_ctor_val) ctor not found"

(* Returns the recursive function associated with this structurally decreasing variable *)
let check_recursive_arg_match (g:Ctx.t) (e:exp) : id option =
  match e with
  | EVar x -> Ctx.fetch_dec_fn x g
  | _ -> None

(* Distributes the evidence according to the constructors that e evaluates to.
 * For each evidence (env, v), if env(v) ~~> C (...), then (env, v) is filed
 * under the C bucket. *)
let distribute_constraints (s:Sig.t) (g:Ctx.t) (e:exp) (ctors:ctor_val list) : synth_branch list =
  if List.length ctors = 0 then [] else
  let dt = List.hd_exn ctors |> datatype_of_ctor_val s in
    Sig.gather_ctors dt s |> List.map ~f:begin fun (c, (ts, _)) ->
      let p  = make_pattern s g c in
      let g' = begin match check_recursive_arg_match g e with
        | Some f -> Ctx.gather_binders s p |> Ctx.mark_as_decreasing f
        | None -> Ctx.gather_binders s p
      end
  in
  let ids = ids_of_pat p in
  let constrs =
    List.filter ~f:(fun (c', _, _, _) -> c = c') ctors
      |> List.fold_left ~f:(fun acc (_, args, env', v) ->
          let env' = (List.zip_exn ids args) @ env' in
          (env', v) :: acc) ~init:[]
  in
    (c, p, Ctx.append g' g, constrs)
  end

(* Returns true if the given synth_branches are an adequate distribution
 * of the evidence as determined by distribute_constraints. *)
let is_adequate_distribution (bs:synth_branch list) : bool =
  let count = List.fold_left
    ~f:(fun acc (_, _, _, l) -> if List.length l > 0 then acc + 1 else acc)
    ~init:0 bs
  in
  count = List.length bs

(***** }}} *****)

(* Creates a split tree for the given synthesis problem. *)
let rec create_rtree (s:Sig.t) (g:Ctx.t) (env:env) (t:typ) (vs:evidence list) (matches_count:int) : rtree =
  { t         = t
  ; sz        = 1
  ; lambdas   = 1
  ; g         = g
  ; vs        = vs
  ; es        = []
  ; timed_out = false
  ; alts      = create_alts s g env t vs matches_count
  ; matches   =
    if matches_count > 0 then
      (* NOTE: if we are at arrow type, don't bother synthesizing matches. *)
      let ms =
        match t with
        | TArr _ -> []
        | _ -> create_matches s g env t vs matches_count 1 !scrutinee_size_lim
      in
      Some (!scrutinee_size_lim, ms)
    else
      None
  }

(* Creates a single match for the given synthesis problem and scrutinee expression. *)
and create_match_one (s:Sig.t) (g:Ctx.t) (env:env) (t:typ)
                     (vs:evidence list) (sv:scrutinee_val) (matches:int) : rmatch option =
  (* NOTE: If evaluation of the potential scrutinee results in no evidence
   * generated (because all examples resulted in bad pattern matches), then
   * reject this scrutinee immediately. *)
  let ctorvals = evaluate_to_ctors sv in
  if List.length ctorvals = 0 then None else
  let branches = distribute_constraints s g sv.e ctorvals in
  if not (is_adequate_distribution branches) then None else
  let trees = List.map ~f:(fun (c, p, g, vs) -> (p, create_rtree s g env t vs (matches-1))) branches in
  Some (sv.e, trees)

(* Creates match nodes for the given synthesis problem. *)
and create_matches (s:Sig.t) (g:Ctx.t) (env:env) (t:typ)
                   (vs:evidence list) (matches:int)
                   (scrutinee_min:int) (scrutinee_max:int) : rmatch list =
  if matches <= 0 then [] else
  let ts = Sig.gather_datatypes s in
  Util.rangen scrutinee_min scrutinee_max
    |> Rope.of_list
    |> Rope.concat_map ~f:begin fun n ->
      Rope.concat_map ~f:begin fun tscrut ->
        let svs = Gen.gen_eexp Timeout.unlimited s g tscrut (Gen.gen_metric n 1)
          |> Rope.map ~f:(evaluate_to_values s env vs)
        in
        Rope.map ~f:(fun sv -> create_match_one s g env t vs sv matches) svs |> Rope.filter_map ~f:Fn.id
      end (Rope.of_list ts)
    end
    |> Rope.to_list

(* Creates (type-directed) split nodes for the given synthesis problem. *)
and create_alts (s:Sig.t) (g:Ctx.t) (env:env) (t:typ) (vs:evidence list) (matches:int) : rnode list =
  let head_ctors_match v1 v2 =
    match (v1, v2) with
    | (VCtor (c, _), VCtor (c', _)) -> c = c'
    | _ -> false
  in
  let extract_ctor v =
    match v with
    | VCtor (c, _) -> c
    | _ -> failwith (Printf.sprintf "(extract_ctor) constructor expected: %s" (Pp.pp_value v))
  in
  match t with
  | TBase _ ->
    let values = List.map ~f:snd vs in
    if List.length values > 0 && Util.all_eq head_ctors_match values then
      let c       = extract_ctor (List.hd_exn values) in
      let (ts, _) = Sig.lookup_ctor_exn c s in
      let argc    = List.length ts in
      if argc = 0 then
        [SCtor (c, [])]
      else
        let vsst = List.zip_exn ts (split_ctor_evidence vs) in
        [SCtor (c, List.map ~f:(fun (t, vs) -> create_rtree s g env t vs matches) vsst)]
    else
      []
  | TArr (t1, t2) ->
    let f  = gen_var_base t  |> fresh_id_from_ctx g in
    let x  = gen_var_base t1 |> fresh_id_from_list (f :: Ctx.ids g) in
    let g  = Ctx.append [ (f, (t, [Ctx.BRec x])); (x, (t1, [Ctx.BArg f])) ] g in
    let vs = split_io_evidence f x env vs in
    [SAbs (f, (x, t1), t2, create_rtree s g env t2 vs matches)]

(* Grows the given refinement tree by one level of matches. *)
let rec grow_matches (s:Sig.t) (env:env) (t:rtree) =
  begin match t.matches with
  | None ->
    if List.length t.es = 0 then
      let ms = create_matches s t.g env t.t t.vs 1 1 !scrutinee_size_lim in
      t.matches <- Some (!scrutinee_size_lim, ms)
  | Some (_, ms) -> List.iter ~f:(fun (_, bs) -> List.iter ~f:(fun (_, t) -> grow_matches s env t) bs) ms
  end;
  List.iter ~f:(grow_matches_rnode s env) t.alts

and grow_matches_rnode (s:Sig.t) (env:env) (n:rnode) =
  match n with
  | SAbs (_, _, _, t) -> grow_matches s env t
  | SCtor (_, ts) -> List.iter ~f:(grow_matches s env) ts

(* Grows the given refinement tree by expanding the match scrutinees by size k. *)
let rec grow_scrutinees (s:Sig.t) (env:env) (k:int) (t:rtree) =
  begin match t.matches with
  | None -> ()
  | Some (n, ms) ->
    let min = n+1 in
    let max = n+k in
    let ms' = create_matches s t.g env t.t t.vs 1 min max in
    t.matches <- Some (max, ms @ ms');
    List.iter ~f:(fun (_, bs) -> List.iter ~f:(fun (_, t) -> grow_scrutinees s env k t) bs) ms
  end;
  List.iter ~f:(grow_scrutinees_rnode s env k) t.alts

and grow_scrutinees_rnode (s:Sig.t) (env:env) (k:int) (n:rnode) =
  match n with
  | SAbs (_, _, _, t) -> grow_scrutinees s env k t
  | SCtor (_, ts) -> List.iter ~f:(grow_scrutinees s env k) ts

(***** }}} *****)

(***** Refinement tree manipulation {{{ *****)

let check_exp (e:exp) (env_global:env) (env_local:env) (v:value) : bool =
  try
    v = Eval.eval (env_local @ env_global) e
  with Eval.Eval_error msg ->
    if not !incomplete_constraints_flag then begin
      incomplete_constraints_flag := true;
      Printf.eprintf "Warning: incomplete constraint set given\n%s\n" msg;
      flush_all ()
    end; false

let satisfies_evidence (e:exp) (env_global:env) (vs:evidence list) : bool =
  List.for_all ~f:(fun (env_local, v) -> check_exp e env_global env_local v) vs

(***** update_exps: e-guesses at each node in the rtree {{{ *****)

let rec update_exps ?(short_circuit = true) (timeout:float) (s:Sig.t) (env:env) (t:rtree) =
  let do_if_no_exp (f:unit -> unit) =
    if not short_circuit || List.length t.es = 0 then f ()
  in
  (* Update this node's expressions... *)
  do_if_no_exp (fun _ ->
    (* NOTE: only generate expressions at this node if it is at base type... *)
    begin match t.t with
    | TArr _ -> ()
    | _ ->
      (* ...and we have not exceeded the max eguess size nor timed out yet. *)
      if (not t.timed_out) && t.sz <= !max_eguess_size then try
        let es = Gen.gen_eexp (Timeout.create timeout) s t.g t.t (Gen.gen_metric t.sz 1) in
        es |> Rope.iter ~f:begin fun e ->
            (* TODO: probably want to short-circuit walking through gen_eexp results as well... *)
            if short_circuit then
              match t.es with
              | [] -> if satisfies_evidence e env t.vs then t.es <- [e]
              | _ :: _ -> ()
            else
              if satisfies_evidence e env t.vs then t.es <- e :: t.es
            end;
        t.sz <- t.sz + 1
      with
        Timeout.Timeout_exception -> begin
          do_if_verbose (fun _ ->
            Printf.printf "Timeout occurred while guessing, size = %d\n%!" t.sz);
          t.timed_out <- true
        end

    end);
  (* ...if we didn't update this tree's exp then recursively update it's children. *)
  do_if_no_exp (fun _ -> begin
    update_exps_matches ~short_circuit:short_circuit timeout s env t.matches;
    List.iter ~f:(update_exps_node ~short_circuit:short_circuit timeout s env) t.alts;
  end)

and update_exps_matches ?(short_circuit = true) (timeout:float) (s:Sig.t)
                        (env:env) (mopt:rtree_matches) =
  match mopt with
  | None -> ()
  | Some (_, ms) -> List.iter ~f:(update_exps_rmatch ~short_circuit:short_circuit timeout s env) ms

and update_exps_rmatch ?(short_circuit = true) (timeout:float) (s:Sig.t) (env:env) (m:rmatch) =
  let (_, bs) = m in
  List.iter ~f:(fun (_, t) -> update_exps ~short_circuit:short_circuit timeout s env t) bs

and update_exps_node ?(short_circuit = true) (timeout:float) (s:Sig.t) (env:env) (n:rnode) =
  match n with
  | SAbs (_, _, _, t) -> update_exps ~short_circuit:short_circuit timeout s env t
  | SCtor (_, ts) -> List.iter ~f:(update_exps ~short_circuit:short_circuit timeout s env) ts

(***** }}} *****)

(***** reset_timeouts: resets the timeout flag in the rtree {{{ *****)

let rec reset_timeouts (t:rtree) = begin
  t.timed_out <- false;
  match t.matches with
  | None -> ()
  | Some (_, ms) -> begin
    List.iter ~f:(fun (_, bs) ->
      List.iter ~f:(fun (_, t) -> reset_timeouts t) bs) ms
    end;
  List.iter ~f:reset_timeouts_alts t.alts
end

and reset_timeouts_alts (n:rnode) =
  match n with
  | SAbs (_, _, _, t) -> reset_timeouts t
  | SCtor (_, ts) -> List.iter ~f:reset_timeouts ts

(***** }}} *****)

(***** propogate_exps: tries to construct exps from rtree sub-children {{{ *****)

let rec propogate_exps ?short_circuit:(sc = true) (t:rtree) : exp list =
  if sc && List.length t.es > 0 then
    t.es
  else
    (* NOTE: Prioritize lambdas, matches, and then constructors, in that order. *)
    let es = t.es
      @ (List.filter ~f:(function SAbs _ -> true | _ -> false) t.alts
        |> List.concat_map ~f:(propogate_exps_node ~short_circuit:sc))
      @ propogate_exps_matches ~short_circuit:sc t.matches
      @ (List.filter ~f:(function SCtor _ -> true | _ -> false) t.alts
        |> List.concat_map ~f:(propogate_exps_node ~short_circuit:sc))
    in
      t.es <- es;
      es

and propogate_exps_matches ?short_circuit:(sc = true) (mopt:rtree_matches) : exp list =
  match mopt with
  | None -> []
  | Some (_, ms) -> List.concat_map ~f:(propogate_exps_rmatch ~short_circuit:sc) ms

and propogate_exps_rmatch ?short_circuit:(sc = true) (m:rmatch) : exp list =
  let (e, bs)  = m in
  let (ps, ts) = List.unzip bs in
  List.map ~f:(propogate_exps ~short_circuit:sc) ts
    |> Util.combinations
    |> List.map ~f:(fun es -> EMatch (e, List.zip_exn ps es))

and propogate_exps_node ?short_circuit:(sc = true) (n:rnode) : exp list =
  match n with
  | SAbs (f, x, ty, t) ->
    List.map ~f:(fun e -> EFix (f, x, ty, e)) (propogate_exps ~short_circuit:sc t)
  | SCtor (c, ts) ->
    if List.length ts > 0 then
      List.map ~f:(propogate_exps ~short_circuit:sc) ts
        |> Util.combinations
        |> List.map ~f:(fun es -> ECtor (c, es))
    else
      [ECtor (c, [])]

(***** }}} *****)

(***** }}} *****)
