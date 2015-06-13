#use "bool.decls"

let bool_impl : bool -> bool -> bool |>
  { True => True => True
  ; True => False => False
  ; False => True => True
  ; False => False => True } = ?
