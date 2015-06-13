type nat =
  | O
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let snoc : list -> nat -> list =
  let rec f (l:list) : nat -> list =
    fun (n:nat) ->
      match l with
      | Nil -> Cons (n, Nil)
      | Cons (x, xs) -> Cons (x, f xs n)
  in
    f
;;

let list_rev_snoc : list -> list |>
  { [] => []
  | [0] => [0]
  | [1] => [1]
  | [0;1] => [1;0]
  | [0;0;1] => [1;0;0]
  } = ?
