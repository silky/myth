(* CONTAINS CONTEXT FUNCTION *)

type bool =
  | True
  | False

(* NO BOOL OPERATIONS *)

type nat =
  | O
  | S of nat

let rec plus (n1:nat) (n2:nat) : nat =
  match n1 with
  | O -> n2
  | S (n1) -> S (plus n1 n2)
;;

let rec div2 (n:nat) : nat =
  match n with
  | O -> O
  | S (n1) -> match n1 with
    | O -> O
    | S (n2) -> S (div2 n2)
;;

type list =
  | Nil
  | Cons of nat * list

let rec append (l1:list) (l2:list) : list =
  match l1 with
  | Nil -> l2
  | Cons (x, l1) -> Cons (x, append l1 l2)
;;

type cmp =
  | LT
  | EQ
  | GT

let rec compare (n1 : nat) (n2 :nat) : cmp =
  match n1 with
  | O -> (match n2 with
           | O -> EQ
           | S (m) -> LT
         )
  | S (m1) ->
      ( match n2 with
      | O -> GT
      | S (m2) -> (compare m1 m2) )
;;

type btree =
  | Leaf
  | Node of tree * bool * tree

type ntree =
  | Leaf
  | Node of tree * nat * tree


let bool_band : bool -> bool -> bool |>
  { True => True => True
  ; True => False => False
  ; False => True => False
  ; False => False => False } = ?
