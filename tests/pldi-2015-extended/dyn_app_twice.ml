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

let pred (d:dyn) : dyn =
  match d with
  | Error -> Error
  | Base ( n ) ->
    (match n with
    | O -> Base ( O )
    | S ( n ) -> Base ( n ))
  | Dyn ( f ) -> Error
;;

let dyn_app_twice : dyn -> dyn -> dyn |>
{
  Dyn (succ) => ( Base( 0 ) => Base( 2 )
                | Base( 1 ) => Base( 3 ) )
| Dyn (pred) => ( Base( 0 ) => Base( 0 )
                | Base( 1 ) => Base( 0 ) )
| Error => Error => Error
| Base( 0 ) => Error => Error
} = ?
