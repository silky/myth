#use "context.decls"

let list_hd : list -> nat |>
  { [] => 0
  | [0] => 0
  | [1] => 1
  } = ?
