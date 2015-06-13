type nat =
  | O
  | S of nat

type list =
  | Nil
  | Cons of nat * list

type tree =
  | Leaf
  | Node of tree * nat * tree

let rec append (l1:list) (l2:list) : list =
  match l1 with
  | Nil -> l2
  | Cons (x, l1) -> Cons (x, append l1 l2)
;;

let tree_postorder : tree -> list |>
{  Leaf => []
| Node (Leaf, 1, Leaf) => [1]
| Node (Leaf, 2, Leaf) => [2]
| Node (Node (Leaf, 1, Leaf), 2, Leaf) => [1;2]
| Node (Leaf, 1, Node (Leaf, 2, Leaf)) => [2;1]
| Node (Node (Leaf, 1, Leaf), 0, Node (Leaf, 2, Leaf) ) => [1;2;0]
| Node (Node (Leaf, 2, Leaf), 0, Node (Leaf, 1, Leaf) ) => [2;1;0]
| Node (Node (Node (Leaf, 2, Leaf), 0, Node (Leaf, 1, Leaf) ), 0, Leaf) => [2;1;0;0]
| Node (Leaf, 2, Node (Node (Leaf, 2, Leaf), 0, Node (Leaf, 1, Leaf) )) => [2;1;0;2]
} = ?
