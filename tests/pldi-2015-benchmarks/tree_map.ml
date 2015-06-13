type nat =
  | O
  | S of nat

type list =
  | Nil
  | Cons of nat * list

type tree =
  | Leaf
  | Node of tree * nat * tree

let rec div2 (n:nat) : nat =
  match n with
  | O -> O
  | S (n1) -> match n1 with
    | O -> O
    | S (n2) -> S (div2 n2)
;;

let rec inc (n:nat) : nat =
  S( n )
;;


let tree_map : (nat -> nat) -> tree -> tree |>
{ div2 => ( Leaf => Leaf
          | Node (Leaf, 0, Leaf) => Node (Leaf, 0, Leaf)
          | Node (Leaf, 2, Leaf) => Node (Leaf, 1, Leaf)
          | Node (Node (Leaf, 2, Leaf), 2, Leaf) => Node (Node (Leaf, 1, Leaf), 1, Leaf) 
          | Node (Leaf, 1, Node (Leaf, 2, Leaf)) => Node (Leaf, 0, Node (Leaf, 1, Leaf)) )
| inc =>  ( Leaf => Leaf
          | Node (Leaf, 0, Leaf) => Node (Leaf, 1, Leaf) )
} = ?
