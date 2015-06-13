open Synml

let test = "tests/ast/arith.ml"

let _ =
  parse_file test |> synthesize_prog |> ignore
