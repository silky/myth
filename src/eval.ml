open Core.Std
open Consts
open Lang
open Pp

exception Eval_error of string

(***** Helpers {{{ *****)

let rec find_first_branch (c:id) (bs:branch list) : branch =
  match bs with
  | [] -> raise @@ Internal_error "Constructor not found during matching"
  | (p, e)::bs ->
    begin match p with
    | PCtor (c', _) -> if c = c' then (p, e) else find_first_branch c bs
    end

let gather_vars (p:pat) : id list =
  match p with
  | PCtor (_, xs) -> xs

(***** }}} *****)

(***** Evaluation Cache {{{ *****)

module GTS : sig
  type t = { env : env; e : exp }
  val make_key : env -> exp -> t
  include Hashable.S with type t := t
end = struct
  module T = struct
    type t = { env : env; e : exp }
    let make_key (env:env) (e:exp) = { env = env; e = e }
    let hash k = Hashtbl.hash k
    let compare = compare
    let sexp_of_t (_:t) : Sexp.t = failwith "GTS.sexp_of_t unimplemented"
    let t_of_sexp (_:Sexp.t) : t = failwith "GTS.t_of_sexp unimplemented"
  end
  include T
  include Hashable.Make(T)
end

let lookup_tables : bool ref = ref true ;;
let memo_eval_tbl : (GTS.t, value) Hashtbl.t = Hashtbl.create ~hashable:GTS.hashable ()

let find_in_table tbl key =
  if !eval_lookup_tables then
    Hashtbl.find tbl key
  else
    None

(***** }}} *****)

let rec eval (env:env) (e:exp) : value =
  let key = GTS.make_key env e in
  match find_in_table memo_eval_tbl key with
  | Some ans -> ans
  | None ->
    let ans = begin match e with
    | EVar x -> List.Assoc.find_exn env x
    | EApp (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
        begin match v1 with
        | VFun (x, e, closure) ->
          eval ((x, v2) :: !closure) e
        | VPFun vps ->
          begin match Util.find_first (fun (v1, _) -> v1 = v2) vps with
          | Some (_, v) -> v
          | None ->
              raise @@ Eval_error (Printf.sprintf "Non-matched value %s found with partial function:\n%s"
                (Pp.pp_value v2) (Pp.pp_value v1))
          end
        | _ -> raise @@ Eval_error "Non-function value found in application"
        end
    | EFun ((x, _), e) -> VFun (x, e, ref env)
    | ELet (f, is_rec, xs, t, e1, e2) ->
        let count = List.length xs in
        if count = 0 then
          (* Value binding *)
          let v1 = eval env e1 in
          eval ((f, v1) :: env) e2
        else
          (* Function binding *)
          let rec binding_to_funs xs e =
            match xs with
            | []      -> e
            | x :: xs -> EFun (x, binding_to_funs xs e)
          in
          let (x1, t1) = List.hd_exn xs in
          let fn = if is_rec then
            let e1 = binding_to_funs (List.tl_exn xs) e1 in
              EFix (f, (x1, t1), (List.map ~f:snd (List.tl_exn xs)) @ [t] |> types_to_arr, e1)
          else
            binding_to_funs xs e1
          in
          eval ((f, eval env fn) :: env) e2
    | ECtor (c, es) -> VCtor (c, List.map ~f:(eval env) es)
    | EMatch (e, bs) ->
        let v = eval env e in
        begin match v with
        | VCtor (c, vs) ->
          let (p, e) = find_first_branch c bs in
          let ids    = gather_vars p in
          eval ((List.zip_exn ids vs) @ env) e
        | _ -> raise @@ Eval_error (Printf.sprintf "Non-datatype value found in match: %s" (Pp.pp_exp e))
        end
    | EPFun ios -> VPFun (List.map ~f:(fun (e1, e2) -> (eval env e1, eval env e2)) ios)
    | EFix (f, (x, _), _, e) ->
        let closure = ref [] in
        let v = VFun (x, e, closure) in
        closure := (f, v) :: env; v
    end
  in
    (Hashtbl.set memo_eval_tbl key ans; ans)

let gen_init_env (ds:decl list) : env =
  let process env = function
    | DData _ -> env
    | DLet (f, is_rec, xs, t, e) ->
      if List.length xs = 0 then
        (* Value binding *)
        let v = eval env e in
        (f, v) :: env
      else
        (* Function binding *)
        let v = eval env (ELet (f, is_rec, xs, t, e, EVar f)) in
        (f, v) :: env
  in
    List.fold_left ~f:process ~init:[] ds

let gen_init_evidence (env:env) (es:exp list) : evidence list =
  List.map ~f:(fun e -> ([], eval env e)) es
