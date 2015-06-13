#use "context.decls"

let inc (n:nat) : nat = S (n) ;;

let tree_map : (nat -> nat) -> tree -> tree |>
{ div2 => ( Leaf => Leaf
          | Node (Leaf, 0, Leaf) => Node (Leaf, 0, Leaf)
          | Node (Leaf, 2, Leaf) => Node (Leaf, 1, Leaf)
          | Node (Node (Leaf, 2, Leaf), 2, Leaf) => Node (Node (Leaf, 1, Leaf), 1, Leaf)
          | Node (Leaf, 1, Node (Leaf, 2, Leaf)) => Node (Leaf, 0, Node (Leaf, 1, Leaf)) )
| inc =>  ( Leaf => Leaf
          | Node (Leaf, 0, Leaf) => Node (Leaf, 1, Leaf) )
} = ?
