type bool =
  | True
  | False

type tree =
  | Leaf
  | Node of tree * bool * tree

type list =
  | Nil
  | Cons of bool * list

let rec append (l1:list) (l2:list) : list =
  match l1 with
  | Nil -> l2
  | Cons (x, l1) -> Cons (x, append l1 l2)
;;

let tree_collect_leaves : tree -> list |>
  { Leaf => []
  | Node (Leaf, True, Leaf) => [True]
  | Node (Leaf, False, Leaf) => [False]
  | Node (Node (Leaf, True, Leaf), False, Leaf) => [True; False]
  | Node (Node (Leaf, False, Leaf), True, Leaf) => [False; True]
  | Node (Leaf, False, Node (Leaf, True, Leaf)) => [False; True]
  } = ?
