type cmp =
  | LT
  | EQ
  | GT

let rec compare (n1:nat) (n2:nat) : cmp =
  match n1 with
  | O ->
    (match n2 with
    | O -> EQ
    | S (m) -> LT)
  | S (m1) ->
    (match n2 with
    | O -> GT
    | S (m2) -> (compare m1 m2))
;;
