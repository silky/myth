#use "context.decls"

let list_length : list -> nat |>
  { [] => 0
  | [0] => 1
  | [0;0] => 2 } = ?
