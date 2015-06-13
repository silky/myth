type nat =
  | O
  | S of nat

type bool =
  | True
  | False

type list =
  | Nil
  | Cons of bool * list

let list_even_parity : list -> bool |>
    { [] => True
    | [ False ] => True
    | [ True  ] => False
    | [ False ; False ] => True
    | [ False ; True ] => False
    | [ True  ; False ] => False
    | [ True  ; True ] => True
    } = ?
