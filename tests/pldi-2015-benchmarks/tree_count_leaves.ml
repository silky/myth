type bool =
  | True
  | False

type tree =
  | Leaf
  | Node of tree * bool * tree

type nat =
  | O
  | S of nat

let rec sum (n1:nat) (n2:nat) : nat =
  match n1 with
  | O -> n2
  | S (n1) -> S (sum n1 n2)
;;

let tree_count_leaves : tree -> nat |>
  { Leaf => 1
  | Node (Leaf, True, Leaf) => 2
  | Node (Node (Leaf, True, Leaf), True, Leaf) => 3
  | Node (Leaf, True, Node (Leaf, True, Leaf)) => 3
  | Node (Node (Node (Leaf, True, Leaf), True, Leaf), True, Leaf) => 4
  | Node (Node (Leaf, True, Leaf), True, Node (Leaf, True, Leaf)) => 4
  | Node (Node (Leaf, True, Leaf), True, Node (Node (Leaf, True, Leaf), True, Node (Leaf, True, Leaf))) => 6
  } = ?
