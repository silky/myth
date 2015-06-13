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
      | S (n1p) -> S (sum n1p n2)
;;

let tree_nodes_at_level : tree -> nat -> nat |>
  { Leaf =>
    ( 0 => 0
    | 1 => 0
    | 2 => 0
    | 3 => 0
    )
  | Node (Leaf, True, Leaf) =>
    ( 0 => 1
    | 1 => 0
    | 2 => 0
    | 3 => 0
    )
  | Node (Node (Leaf, True, Leaf), True, Leaf) =>
    ( 0 => 1
    | 1 => 1
    | 2 => 0
    | 3 => 0
    )
  | Node (Leaf, True, Node (Leaf, True, Leaf)) =>
    ( 0 => 1
    | 1 => 1
    | 2 => 0
    | 3 => 0
    )
  | Node (Node (Leaf, True, Leaf), True, Node (Leaf, True, Leaf)) =>
    ( 0 => 1
    | 1 => 2
    | 2 => 0
    | 3 => 0
    )
  | Node (Node (Node(Leaf, True, Leaf), True, Node (Leaf, True, Leaf)), True, Leaf) =>
    ( 0 => 1
    | 1 => 1
    | 2 => 2
    | 3 => 0)
  } = ?
