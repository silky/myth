#use "bool.decls"

let bool_band : bool -> bool -> bool |>
  { True => True => True
  ; True => False => False
  ; False => True => False
  ; False => False => False } = ?
