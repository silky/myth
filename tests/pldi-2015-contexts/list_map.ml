#use "context.decls"

let zero (n:nat) : nat = O ;;
let inc (n:nat): nat = S (n) ;;

let list_map : (nat -> nat) -> list -> list |>
  { inc => ( [] => []
            | [0] => [1]
            | [0; 0] => [1; 1]
            | [1] => [2]
            | [1; 1] => [2; 2] )
  | zero => ( [] => []
            | [0] => [0]
            | [0; 0] => [0; 0] )
  } = ?
