open Core

type 'a t =
  | Concat of 'a t * 'a t
  | OfList of 'a list

let rec map (r : 'a t) ~f : 'b t =
  match r with
  | Concat(lr, rr) -> 
    let ml = map lr f in
    let mr = map rr f in
    begin match (ml, mr) with
      | OfList [], OfList [] -> OfList []
      | OfList [], _ -> mr
      | _, OfList [] -> ml
      | _, _ -> Concat(ml, mr)
    end
  | OfList l -> OfList (List.map l ~f)

let rec iter (r : 'a t) ~f : unit =
  match r with
  | Concat(lr, rr) -> iter lr f; iter rr f
  | OfList l -> List.iter l ~f

let rec to_list (r : 'a t) : 'a list =
  match r with
  | Concat(lr, rr) -> (to_list lr) @ (to_list rr)
  | OfList l -> l

(* TODO: Factor these implementations to share common code *)

let rec concat_map (r : 'a t) ~f : 'b t =
  match r with
  | Concat(lr, rr) -> 
    let ml = concat_map ~f lr in
    let mr = concat_map ~f rr in
    begin match (ml, mr) with
      | OfList [], OfList [] -> OfList []
      | OfList [], _ -> mr
      | _, OfList [] -> ml
      | _, _ -> Concat(ml, mr)
    end
  | OfList l -> List.fold_left ~f:(fun acc x -> Concat((f x), acc)) ~init:(OfList []) l

let rec filter (r : 'a t) ~f : 'b t =
  match r with
  | Concat(lr, rr) ->
    let ml = filter ~f lr in
    let mr = filter ~f rr in
    begin match (ml, mr) with
      | OfList [], OfList [] -> OfList []
      | OfList [], _ -> mr
      | _, OfList [] -> ml
      | _, _ -> Concat(ml, mr)
    end
  | OfList l -> OfList (List.filter ~f l)


let rec filter_map (r : 'a t) ~f : 'b t =
  match r with
  | Concat(lr, rr) ->
    let ml = filter_map ~f lr in
    let mr = filter_map ~f rr in
    begin match (ml, mr) with
      | OfList [], OfList [] -> OfList []
      | OfList [], _ -> mr
      | _, OfList [] -> ml
      | _, _ -> Concat(ml, mr)
    end
  | OfList l -> OfList (List.filter_map ~f l)

let rec dedup (r:'a t) ~compare:(cc:'a -> 'a -> int) : 'a t =
  match r with
  | Concat (lr, rr) ->
    let ml = dedup ~compare:cc lr in
    let mr = dedup ~compare:cc rr in
    begin match (ml, mr) with
      | OfList [], OfList [] -> OfList []
      | OfList [], _ -> mr
      | _, OfList [] -> ml
      | _, _ -> Concat(ml, mr)
    end
  | OfList l -> OfList (List.dedup_and_sort ~compare:cc l)

let rec cons x r =
  match r with
  | Concat(rr, rl) -> Concat(cons x rr, rl)
  | OfList l -> OfList(x::l)

let empty = OfList []

let of_list l = OfList l

let concat r1 r2 = Concat(r1, r2)

let rec length r =
  match r with
  | Concat(lr, rr) -> (length lr) + (length rr)
  | OfList l -> List.length l

let rec is_empty r =
  match r with
  | Concat(lr, rr) -> (is_empty lr) && (is_empty rr)
  | OfList [] -> true
  | OfList _ -> false


(* Morally:
   Given a list of lists, [[x1;x2;x3];[y1;y2;y3;...];...;[z1;z2;z3]] 
   form all possible combinations by   drawing one element from each list.
   [[x1;y1;...;z1];[x1;y1;...;z2];...;[x3;ym;...;z3]]

   The length of the resulting list is the product of the lengths of the 
   input lists.
*)
let cartesian_product (ls:'a t list) : 'a list t =
  let ll = List.map ~f:to_list ls in
  let rec list_cartesian_product (ls : 'a list list) : 'a list list =
    begin match ls with
      | [] -> [[]]
      | xs::rest ->
        let r = list_cartesian_product rest in
        List.concat_map ~f:(fun x -> List.map ~f:(fun l -> (x::l)) r) xs
    end
  in
  of_list (list_cartesian_product ll)
(*
The following slowed down the cartesian product! 
    if ls = [] then OfList [[]]
    else
      let rec to_lists ls acc =
        begin match ls with
          | [] -> Some acc
          | r::rest -> 
            let lr = to_list r in
            if lr = [] then None
            else to_lists rest (lr::acc)
        end
      in
      match to_lists ls [] with
      | None -> Empty
      | Some ll -> 
        let rec list_cartesian_product (ls : 'a list list) : 'a list list =
          begin match ls with
            | [] -> [[]]
            | xs::rest ->
              let r = list_cartesian_product rest in
              List.concat_map ~f:(fun x -> List.map ~f:(fun l -> (x::l)) r) xs
          end
        in
        of_list (list_cartesian_product ll)
*)
