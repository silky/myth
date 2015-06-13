#use "context_bool.decls"

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
