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
  | O -> ( match n2 with
           | O -> EQ
           | S (m) -> LT
         )
  | S (m1) ->
      ( match n2 with
      | O -> GT
      | S (m2) -> (compare m1 m2) )
;;

let rec insert (l : list) (n :nat) : list =
  match l with
  | Nil -> Cons(n, Nil)
  | Cons(m, tl) ->
    (match compare n m with
     | LT -> Cons (n, Cons(m, tl))
     | EQ -> l
     | GT -> Cons (m, insert tl n)
    )
;;

let list_sort_sorted_insert : list -> list |>
 { [] => []
 | [0] => [0]
 | [1] => [1]
 | [0;0] => [0]
 | [1;0] => [0;1]
 | [1;1] => [1]
 | [0;1;1] => [0;1]
 } = ?


