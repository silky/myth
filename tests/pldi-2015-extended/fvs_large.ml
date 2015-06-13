type nat =
  | O
  | S of nat

type list =
  | Nil
  | Cons of nat * list

type binop =
  | Add
  | Sub
  | Mul
  | Div

type exp =
  | Unit
  | BVar of nat
  | FVar of nat
  | Lam of nat * exp
  | App of exp * exp
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp
  | Inl of exp
  | Inr of exp
  | Match of exp * nat * exp * nat * exp
  | Const of nat
  | Binop of exp * binop * exp

let rec append (l1:list) (l2:list) : list =
  match l1 with
  | Nil -> l2
  | Cons (x, l1p) -> Cons (x, append l1p l2)
;;

let fvs_large : exp -> list |>
  { Unit => []
  | BVar (0) => [0]
  | BVar (1) => [1]
  | BVar (2) => [2]
  | FVar (0) => []
  | Lam (0, Unit) => []
  | Lam (0, BVar (1)) => [1]
  | App (Unit, Unit) => []
  | App (BVar (0), Unit) => [0]
  | App (Unit, BVar (1)) => [1]
  | App (BVar (0), BVar (1)) => [0; 1]
  | Fst (Unit) => []
  | Fst (BVar (1)) => [1]
  | Snd (Unit) => []
  | Snd (BVar (1)) => [1]
  | Pair (Unit, Unit) => []
  | Pair (BVar (0), Unit) => [0]
  | Pair (Unit, BVar (1)) => [1]
  | Pair (BVar (0), BVar (1)) => [0; 1]
  | Inl (Unit) => []
  | Inl (BVar (1)) => [1]
  | Inr (Unit) => []
  | Inr (BVar (1)) => [1]
  | Match (Unit, 0, Unit, 1, Unit) => []
  | Match (BVar (0), 0, Unit, 1, Unit) => [0]
  | Match (Unit, 0, BVar (1), 1, Unit) => [1]
  | Match (Unit, 0, Unit, 1, BVar (2)) => [2]
  | Match (BVar (2), 0, BVar (1), 1, BVar (0)) => [2; 1; 0]
  | Const (0) => []
  | Binop (BVar (0), Add, Unit) => [0]
  | Binop (Unit, Add, BVar (1)) => [1]
  } = ?
