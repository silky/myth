type id =
  | A
  | B
  | C

type nat =
  | O
  | S of nat

type list =
  | Nil
  | Cons of id * list

type exp =
  | EBVar of nat
  | EFVar of id
  | EApp  of exp * exp

let rec append (l1:list) (l2:list) : list =
  match l1 with
  | Nil -> l2
  | Cons (x, l1p) -> Cons (x, append l1p l2)
;;

let fvs_small : exp -> list |>
  { EBVar (0) => []
  | EFVar (A) => [A]
  | EFVar (B) => [B]
  | EApp (EBVar (0), EBVar (0)) => []
  | EApp (EFVar (A), EBVar (0)) => [A]
  | EApp (EBVar (0), EFVar (A)) => [A]
  } = ?
