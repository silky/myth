type nat =
  | O
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let rec fold (l:list) (f:list -> nat -> list) (acc:list) : list =
  match l with
  | Nil -> acc
  | Cons (x, l) -> fold l f (f acc x)
;;


let snoc : list -> nat -> list =
  let rec f (l:list) : nat -> list =
    fun (n:nat) ->
      match l with
      | Nil -> Cons (n, Nil)
      | Cons (x, xs) -> Cons (x, f xs n)
  in
    f
;;

let list_rev_fold : list -> list |>
  { [] => []
  | [0] => [0]
  | [1] => [1]
  | [0;1] => [1;0]
  | [0;0;1] => [1;0;0]
  } = ?
