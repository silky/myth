type nat =
  | O
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let rec append (l1:list) (l2:list) : list =
  match l1 with
  | Nil -> l2
  | Cons (x, l1) -> Cons (x, append l1 l2)
;;

let list_rev_append : list -> list |>
  { [] => []
  | [0] => [0]
  | [1] => [1]
  | [0;1] => [1;0]
  | [0;0;1] => [1;0;0]
  } = ?
