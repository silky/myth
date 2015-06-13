(* CONTAINS CONTEXT FUNCTIONS *)

type bool =
  | True
  | False

let rec andb (n1:bool) (n2:bool) : bool =
  match n1 with
  | True -> n2
  | False -> False
;;

let rec orb (n1:bool) (n2:bool) : bool =
  match n1 with
  | True -> True
  | False -> n2
;;

type nat =
  | O
  | S of nat

(* NO NAT FUNCTIONS *)

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

let nat_add : nat -> nat -> nat |>
  { 0 => ( 0 => 0
         | 1 => 1
         | 2 => 2 )
  | 1 => ( 0 => 1
         | 1 => 2
         | 2 => 3 )
  | 2 => ( 0 => 2
         | 1 => 3
         | 2 => 4 )
  } = ?
