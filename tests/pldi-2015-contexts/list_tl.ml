#use "context.decls"

let list_tl : list -> list |>
  { [] => []
  | [0] => []
  | [0; 0] => [0] } = ?
