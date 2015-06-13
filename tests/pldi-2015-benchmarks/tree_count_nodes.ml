type nat =
  | O
  | S of nat

type tree =
  | Leaf
  | Node of tree * nat * tree

let rec sum (n1:nat) (n2:nat) : nat =
  match n1 with
  | O -> n2
  | S (n1) -> S (sum n1 n2)
;;

let tree_count_nodes : tree -> nat |>
  { Leaf => 0
  | Node(Leaf, 0, Leaf) => 1
  | Node(Node(Leaf, 0, Leaf), 0, Leaf) => 2
  | Node(Leaf, 0, Node(Leaf, 0, Leaf)) => 2
  | Node(Node(Leaf, 0, Node(Leaf, 0, Leaf)), 0, Leaf) => 3
  | Node(Leaf, 0, Node(Leaf, 0, Node(Leaf, 0, Leaf))) => 3
  } = ?
