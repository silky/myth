type nat =
  | O
  | S of nat

type list =
  | Nil
  | Cons of nat * list

type bool =
  | True
  | False

let rec is_even (n:nat) : bool =
  match n with
  | O -> True
  | S (n1) ->
    match n1 with
    | O -> False
    | S (n2) -> is_even n2
;;

let rec is_nonzero (n:nat) : bool =
  match n with
  | O -> False
  | S (n1) -> True
;;

let list_filter : (nat -> bool) -> list -> list |>
{
  is_even => ( [] => []
             | [0] => [0]
             | [1] => []
             | [2] => [2]
             | [0;0] => [0;0]
             | [0;1] => [0] )
| is_nonzero => ( [] => []
                | [0] => [] )
} = ?
