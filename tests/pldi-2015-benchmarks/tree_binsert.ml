type cmp =
  | CEq
  | CGt
  | CLt

type nat =
  | O
  | S of nat

type tree =
  | Leaf
  | Node of tree * nat * tree

let rec comp_nat (n1:nat) (n2:nat) : cmp =
  match n1 with
  | O -> (match n2 with
          | O -> CEq
          | S (n2) -> CLt)
  | S (n1) -> (match n2 with
              | O -> CGt
              | S (n2) -> comp_nat n1 n2)
;;

let tree_binsert : tree -> nat -> tree |>
  { Leaf => ( 0 => Node (Leaf, 0, Leaf)
            | 1 => Node (Leaf, 1, Leaf)
            | 2 => Node (Leaf, 2, Leaf) )
  | Node (Leaf, 1, Leaf) => ( 0 => Node (Node (Leaf, 0, Leaf), 1, Leaf)
                            | 1 => Node (Leaf, 1, Leaf)
                            | 2 => Node (Leaf, 1, Node (Leaf, 2, Leaf)) )
  | Node (Leaf, 0, Leaf) => ( 0 => Node (Leaf, 0, Leaf)
                            | 1 => Node (Leaf, 0, Node (Leaf, 1, Leaf))
                            | 2 => Node (Leaf, 0, Node (Leaf, 2, Leaf)) )
  | Node (Leaf, 2, Leaf) => ( 0 => Node (Node (Leaf, 0, Leaf), 2, Leaf)
                            | 1 => Node (Node (Leaf, 1, Leaf), 2, Leaf)
                            | 2 => Node (Leaf, 2, Leaf) )
  | Node (Node (Leaf, 0, Leaf), 1, Leaf) => ( 0 => Node (Node (Leaf, 0, Leaf), 1, Leaf)
					    | 1 => Node (Node (Leaf, 0, Leaf), 1, Leaf)
					    | 2 => Node (Node (Leaf, 0, Leaf), 1, Node(Leaf, 2, Leaf))
					    )
  | Node (Leaf, 0, Node (Leaf, 1, Leaf)) => ( 2 => Node (Leaf, 0, Node (Leaf, 1, Node(Leaf, 2, Leaf))) )
  | Node (Node (Leaf, 1, Leaf), 2, Leaf) => ( 0 => Node (Node (Node(Leaf, 0, Leaf), 1, Leaf), 2, Leaf) )
  | Node (Leaf, 1, Node (Leaf, 2, Leaf)) => ( 0 => Node (Node (Leaf, 0, Leaf), 1, Node (Leaf, 2, Leaf))
                                            | 1 => Node (Leaf, 1, Node (Leaf, 2, Leaf)) )
  | Node (Node (Leaf, 1, Leaf), 2, Leaf) => ( 0 => Node (Node (Node(Leaf, 0, Leaf), 1, Leaf), 2, Leaf) )
  } = ?
