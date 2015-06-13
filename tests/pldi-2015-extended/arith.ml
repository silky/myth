(* -matches 2 -scrutinee 8 *)

type nat =
| O
| S of nat

type cmp =
  | LT
  | EQ
  | GT

type exp =
| Const of nat
| Sum of exp * exp
| Prod of exp * exp
| Pred of exp
| Max of exp * exp 
       
let rec sum (n1:nat) (n2:nat) : nat =
  match n1 with
  | O -> n2
  | S (n1) -> S (sum n1 n2)
;;

let rec mult (n1:nat) (n2:nat) : nat =
  match n1 with
  | O -> O
  | S (n1) -> sum n2 (mult n1 n2)
;;

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

let arith : exp -> nat |>
{ Const (0) => 0
| Const (1) => 1                     
| Const (2) => 2
| Sum (Const(2), Const(2)) => 4
| Sum (Const(2), Const(1)) => 3
| Sum (Const(0), Const(2)) => 2
| Prod (Const(0), Const(2)) => 0
| Prod (Const(2), Const(1)) => 2
| Prod (Const(2), Const(2)) => 4
| Prod (Prod(Const(2), Const(2)), Const(2)) => 8
| Prod (Sum(Const(2), Const(1)), Const(2)) => 6
| Pred (Const(0)) => 0
| Pred (Const(1)) => 0
| Pred (Const(2)) => 1
| Max (Const(0), Const(0)) => 0
| Max (Const(0), Const(1)) => 1
| Max (Const(0), Const(2)) => 2
| Max (Const(1), Const(0)) => 1
| Max (Const(1), Const(1)) => 1
| Max (Const(1), Const(2)) => 2
| Max (Const(2), Const(0)) => 2  
| Max (Const(2), Const(1)) => 2
} = ?
