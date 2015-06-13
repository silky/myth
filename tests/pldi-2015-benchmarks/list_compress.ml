type nat =
  | O
  | S of nat

type list =
  | Nil
  | Cons of nat * list

type cmp =
  | LT
  | EQ
  | GT

let rec compare (n1 : nat) (n2 :nat) : cmp =
  match n1 with
  | O -> (match n2 with
           | O -> EQ
           | S (m) -> LT
         )
  | S (m1) ->
      ( match n2 with
      | O -> GT
      | S (m2) -> (compare m1 m2) )
;;

let list_compress : list -> list |>
{   [] => []
  | [0] => [0]
  | [1] => [1]
  | [0;0] => [0]
  | [1;1] => [1]
  | [2;0] => [2;0]
  | [1;0;0] => [1;0]
  | [0;1;1] => [0;1]
  | [2;1;0;0] => [2;1;0]
  | [2;2;1;0;0] => [2;1;0]
  | [2;2;0] => [2;0]
  | [2;2;2;0] => [2;0]
  | [1;2;2;2;0] => [1;2;0] 
} = ?
