type nat =
  | O
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let list_hd : list -> nat |>
  { [] => 0
  | [0] => 0
  | [1] => 1
  } = ?
