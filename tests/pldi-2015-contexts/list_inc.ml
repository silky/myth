#use "context.decls"

let rec map (l:list) (f : nat -> nat) : list =
     match l with
       | Nil -> Nil
       | Cons (n, ls) -> Cons (f n, map ls f)
;;

let list_inc : list -> list |>
  { [] => []
  | [1;2] => [2;3]
  | [0;0] => [1;1]
  | [3;4;5] => [4;5;6]
  } = ?
