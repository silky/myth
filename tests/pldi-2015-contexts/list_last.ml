#use "context.decls"

type natopt =
  | None
  | Some of nat

let list_last : list -> natopt |>
  { [] => None
  | [1] => Some (1)
  | [2] => Some (2)
  | [2; 1] => Some (1)
  | [1; 2] => Some (2)
  | [3; 2; 1] => Some (1)
  } = ?


