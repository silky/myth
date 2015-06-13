open Core.Std

type atom  = int                    (* n, De Bruijn indices *)
type id    = string                 (* x, C, D, Identifiers *)

type typ =                          (* t *)
  | TBase of id                     (* D *)
  | TArr  of typ * typ              (* t1 -> t2 *)

type ctor = id * typ list           (* C t1 .. tn *)

type pat =                          (* p *)
  | PCtor of id * id list           (* C (x1, .., xn) *)

type arg = id * typ                 (* x:t *)

type env = (id * value) list

and decl =
  | DData of id * ctor list                         (* type D = ctors *)
  | DLet  of id * bool * arg list * typ * exp       (* let x [rec] (x1:t1) .. (xn:tn) : t = e *)

and exp =
  | EVar of id                                      (* x *)
  | EApp of exp * exp                               (* e1 e2 *)
  | EFun of arg * exp                               (* fun (x:t1) -> e *)
  | ELet of id * bool * arg list * typ * exp * exp  (* let [rec] (x1:t1) .. (xn:tn) : t = e1 in e2 *)
  | ECtor of id * exp list                          (* C (e1, .., en) *)
  | EMatch of exp * branch list                     (* match e with bs *)
  | EPFun of (exp * exp) list                       (* v11 => v21 | .. | v1n => v2n *)
  | EFix  of id * arg * typ * exp                   (* fix f (x:t1) : t2 = e *)

and branch = pat * exp                              (* | p -> e *)

and value =
  | VCtor of id * value list
  | VFun  of id * exp * env ref
  | VPFun of (value * value) list

type synth_problem = id * typ * exp list       (* let x : t |> V = ? *)

type evidence = env * value

type prog = decl list * synth_problem

exception Internal_error of string

(***** Signatures and Contexts {{{ *****)

module Sig = struct
  type args_t = typ list * id
  type ctor_t = id * args_t
  type t = ctor_t list

  let empty : t = []
  let append : t -> t -> t = (@)

  let make_sig_from_data (d:id) (cs:ctor list) : t =
    List.map ~f:(fun (c, ts) -> (c, (ts, d))) cs

  let rec make_sig (ds:decl list) : t =
    List.fold_left ~f:(fun res d ->
      begin match d with
      | DData (dt, cs) -> List.append (make_sig_from_data dt cs) res
      | _ -> res
      end
    ) ~init:[] ds

  let lookup_ctor : id -> t -> args_t option = Util.lookup
  let lookup_ctor_exn (x:id) (s:t) : args_t =
    Util.lookup x s |> function
    | Some ans -> ans
    | None -> failwith "lookup_ctor_exn: ctor not found"

  let rec restrict (d:id) (s:t) : t =
    match s with
    | []               -> []
    | (c, (ts, d'))::s ->
      if d = d' then (c, (ts, d'))::(restrict d s) else restrict d s

  let rec extract_ctor (c:id) (s:t) : t * args_t =
    ( Util.remove_first (fun (c', _) -> c = c') s
    , match lookup_ctor c s with
      | Some csig -> csig
      | None -> raise @@ Internal_error (Printf.sprintf "Constructor not found: %s" c) )

  let rec gather_datatypes (s:t) : typ list =
    List.fold_left
      ~f:(fun ts (_, (_, dt)) ->
        let t = TBase dt in if List.mem ts t then ts else t::ts)
      ~init:[] s

  let rec gather_ctors (dt:id) (s:t) : ctor_t list =
    List.filter ~f:(fun (_, (_, dt')) -> dt = dt') s
end

module Ctx = struct
  type bindspec =
  | BRec of id      (* Recursive function binding w/ recursive argument *)
  | BArg of id      (* Argument-to-recursive-function binding *)
  | BDec of id      (* Structurally decreasing binding *)

  type t = (id * (typ * bindspec list)) list

  let empty : t = []

  let lookup (x:id) (g:t) : typ option =
    match Util.lookup x g with
    | Some (t, _) -> Some t
    | None -> None

  let append : t -> t -> t = List.append
  let insert_bindspec (x:id) (t:typ) (bs:bindspec list) (g:t) : t = (x, (t, bs)) :: g
  let insert (x:id) (t:typ) (g:t) : t = insert_bindspec x t [] g

  let map (f:(id * typ) -> 'a) (g:t) : 'a list =
    List.map ~f:(fun (x, (t, _)) -> f (x, t)) g

  let ignore_bindspec (f:typ -> bool) : (typ * bindspec list) -> bool =
    fun (t, _) -> f t

  let strip_bindspecs (g:t) : t =
    List.map ~f:(fun (x, (t, _)) -> (x, (t, []))) g

  let restrict (f:(typ * bindspec list) -> bool) (g:t) : t =
    List.filter ~f:(fun (_, (t, bs)) -> f (t, bs)) g

  let ids : t -> id list = List.map ~f:fst

  let filter_ids (f:(typ * bindspec list) -> bool) (g:t) : id list =
    restrict f g |> ids

  let is_in (x:id) (g:t) : bool =
    match lookup x g with
    | Some _ -> true
    | None -> false

  let gather_binders (s:Sig.t) (p:pat) : t =
    match p with
    | PCtor (c, xs) ->
      begin match Sig.lookup_ctor c s with
      | Some (ts, _) ->
        List.zip_exn xs (List.zip_exn ts (Util.replicate (List.length ts) []))
      | None -> raise @@ Internal_error "Constructor not found in signature"
      end

  let gather_types (g:t) : typ list =
    List.fold_left
      ~f:(fun ts (_, (t, _)) -> if List.mem ts t then ts else t::ts)
      ~init:[] g

  let fetch_rec_arg (f:id) (g:t) : id option =
    let rec extract bs =
      match bs with
      | [] -> None
      | (BRec x) :: _ -> Some x
      | _ :: bs -> extract bs
    in
    match Util.lookup f g with
    | Some (_, bs) -> extract bs
    | None -> None

  let fetch_dec_fn (x:id) (g:t) : id option =
    let rec extract bs =
      match bs with
      | [] -> None
      | (BArg f) :: _ -> Some f
      | _ :: bs -> extract bs
    in
    match Util.lookup x g with
    | Some (_, bs) -> extract bs
    | None -> None

  let is_rec_fun (f:id) (g:t) : bool =
    match Util.lookup f g with
    | Some (_, bs) -> List.exists ~f:(function BRec _ -> true | _ -> false) bs
    | None -> false

  let is_dec_arg (x:id) (f:id) (g:t) : bool =
    match Util.lookup x g with
    | Some (_, bs) -> List.mem bs (BDec f)
    | None -> false

  let is_arg (x:id) (g:t) : bool =
    match Util.lookup x g with
    | Some (_, bs) -> List.exists ~f:(function | BArg _ -> true | _ -> false) bs
    | None -> false

  let rec mark_as_decreasing (f:id) (g:t) : t =
    List.map ~f:(fun (x, (t, bs)) -> (x, (t, (BDec f)::(BArg f)::bs))) g

  let size (g:t) : int = List.length g

  let peel (g:t) : ((id * (typ * bindspec list)) * t) option =
    match g with
    | [] -> None
    | (x, (t, bs)) :: g -> Some ((x, (t, bs)), g)
end

let gen_var_base (t:typ) : id =
  match t with
  | TArr (_, _) -> "f"
  | TBase dt -> dt.[0] |> Char.lowercase |> String.make 1

(***** }}} *****)

(***** Miscellaneous operations {{{ *****)

let rec extract_types (t:typ) : typ list =
  match t with
  | TArr (t1, t2) -> t :: List.append (extract_types t1) (extract_types t2)
  | TBase _ -> [t]

let gather_all_types (s:Sig.t) (g:Ctx.t) : typ list =
  List.append (Sig.gather_datatypes s) (Ctx.gather_types g)
    |> List.map ~f:extract_types
    |> List.concat
    |> Util.nub

let is_datatype (t:typ) : bool =
  match t with
  | TBase _ -> true
  | _ -> false

let extract_datatype (t:typ) : id =
  match t with
  | TBase dt -> dt
  | _ -> raise @@ Internal_error "(extract_datatype) Non-datatype given"

let rec size (e:exp) : int =
  match e with
  | EVar x -> 1
  | EApp (e1, e2) -> 1 + size e1 + size e2
  | EFun (_, e) -> 1 + size e
  | ELet (_, _, _, _, e1, e2) -> 1 + size e1 + size e2
  | ECtor (_, es) -> 1 + List.fold_left ~f:(fun n e -> n + size e) ~init:0 es
  | EMatch (e, bs) -> 1 + size e + List.fold_left ~f:(fun n (_, e) -> n + size e) ~init:0 bs
  | EPFun ios -> 1 + List.fold_left ~f:(fun n (e1, e2) -> n + size e1 + size e2) ~init:0 ios
  | EFix (_, _, _, e) -> 1 + size e

let rec examples_count (v:value) : int =
  match v with
  | VPFun ios -> List.fold_left ~f:(fun n (_, v) -> n + examples_count v) ~init:0 ios
  | _ -> 1

let rec free_vars (e:exp) : id list =
  match e with
  | EVar x -> [x]
  | EApp (e1, e2) -> free_vars e1 @ free_vars e2
  | EFun (_, e) -> free_vars e
  | ELet (_, _, _, _, e1, e2) -> free_vars e1 @ free_vars e2
  | ECtor (_, es) -> List.map ~f:free_vars es |> List.concat
  | EMatch (e, bs) ->
    List.map ~f:(fun (_, e) -> free_vars e) bs
      |> List.concat
      |> (@) (free_vars e)
  | EPFun ios ->
    List.fold_left
      ~f:(fun acc (e1, e2) -> acc @ free_vars e1 @ free_vars e2)
      ~init:[] ios
  | EFix (_, _, _, e) -> free_vars e

let ids_of_pat (p:pat) : id list =
  match p with
  | PCtor (_, ids) -> ids

let rec types_to_arr (ts:typ list) =
  match ts with
  | []  -> raise @@ Internal_error "(types_to_arr) empty type list provided"
  | [t] -> t
  | t :: ts -> TArr (t, types_to_arr ts)

let check_recursive_fun (g:Ctx.t) (e:exp) : id option =
  match e with
  | EVar x -> if Ctx.is_rec_fun x g then Some x else None
  | _ -> None

let is_recursive_fun (g:Ctx.t) (e:exp) : bool =
  match check_recursive_fun g e with
  | Some _ -> true
  | None -> false

let fresh_id_from_list (ids:id list) (base:id) : id =
  let rec fresh n =
    let x = Printf.sprintf "%s%d" base n in
    if List.mem ids x then fresh (n+1) else x
  in
  fresh 1

let fresh_id_from_ctx (g:Ctx.t) (base:id) : id = fresh_id_from_list (Ctx.ids g) base

(***** }}} *****)
