type nat =
  | O
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let list_stutter : list -> list |>
  { [] => []
  | [0] => [0;0]
  | [1;0] => [1;1;0;0]
  } = ?
