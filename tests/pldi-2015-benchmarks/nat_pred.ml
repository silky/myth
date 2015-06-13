#use "nat.decls"

let nat_pred : nat -> nat |>
  { O => O
  ; S (O) => O
  ; S (S (O)) => S (O) } = ?
