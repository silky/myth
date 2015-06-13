type nat =
  | O
  | S of nat

type bool =
  | True
  | False

type list =
  | Nil
  | Cons of nat * list

let rec sum (n1:nat) (n2:nat) : nat =
  match n1 with
  | O -> n2
  | S (n1) -> S (sum n1 n2)
;;

let rec is_odd (n:nat) : bool =
  match n with
  | O -> False
  | S (n) ->
    (match n with
    | O -> True
    | S (n1) -> is_odd n1)
;;

let count_odd : nat -> nat -> nat =
  fun (n1:nat) -> fun (n2:nat) ->
    match is_odd n2 with
    | True -> S (n1)
    | False -> n1
;;

let list_fold : (nat -> nat -> nat) -> nat -> list -> nat |>
    { sum => ( 0 => ( [] => 0
                    | [1] => 1
                    | [2; 1] => 3
                    | [3; 2; 1] => 6 )
             | 1 => [] => 1 )
    | count_odd => ( 0 => ( [] => 0
                          | [1] => 1
                          | [2; 1] => 1
                          | [3; 2; 1] => 2 ) )
    } = ?
