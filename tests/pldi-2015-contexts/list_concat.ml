#use "context.decls"

type llist =
  | LNil
  | LCons of list * llist

let list_concat : llist -> list |>
  { LNil => []
  | LCons ([], LNil) => []
  | LCons ([0], LNil) => [0]
  | LCons ([0], LCons([0], LNil)) => [0;0]
  | LCons ([1], LNil) => [1]
  | LCons ([1], LCons([1], LNil)) => [1;1]
  } = ?
