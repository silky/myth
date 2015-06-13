type nat =
   | O
   | S of nat

type dyn =
  | Error
  | Base of nat
  | Dyn of (dyn -> dyn)


let succ (d:dyn) : dyn =
  match d with
  | Error -> Error
  | Base ( n ) -> Base (S(n))
  | Dyn ( f ) -> Error
;;

let id (d:dyn) : dyn = d ;;

let dyn_sum : dyn -> dyn -> dyn |>
{ Dyn (id) =>
  ( Error    => Error
  | Dyn (id) => Error
  | Base (0) => Error
  | Base (1) => Error
  | Base (2) => Error )
| Error =>
  ( Error    => Error
  | Dyn (id) => Error
  | Base (0) => Error
  | Base (1) => Error
  | Base (2) => Error )
| Base (0) =>
  ( Error    => Error
  | Dyn (id) => Error
  | Base (0) => Base (0)
  | Base (1) => Base (1)
  | Base (2) => Base (2) )
| Base (1) =>
  ( Error    => Error
  | Dyn (id) => Error
  | Base (0) => Base (1)
  | Base (1) => Base (2)
  | Base (2) => Base (3) )
| Base (2) =>
  ( Error    => Error
  | Dyn (id) => Error
  | Base (0) => Base (2)
  | Base (1) => Base (3)
  | Base (2) => Base (4) )
} = ?
