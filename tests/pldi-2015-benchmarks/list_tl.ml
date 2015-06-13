type nat =
  | O
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let list_tl : list -> list |>
  { [] => []
  | [0] => []
  | [0; 0] => [0] } = ?
