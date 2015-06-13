#use "context.decls"

let rec fold (l:list) (f:nat -> nat -> nat) (acc:nat) : nat =
  match l with
  | Nil -> acc
  | Cons (x, l) -> fold l f (f acc x)
;;

let list_sum : list -> nat |>
  { [] => 0
  | [1] => 1
  | [2; 1] => 3
  } = ?
