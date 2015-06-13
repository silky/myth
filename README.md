Myth: ML Synthesizer
====================

Requirements
------------
Ocaml > 4.01.0

Quick and Dirty Installation Instructions
-----------------------------------------
opam install core
opam install menhir
make

Example Execution
-----------------
>$ make
ocamlbuild  synml.native
Finished, 1 target (0 cached) in 00:00:00.
+ menhir --ocamlc 'ocamlfind ocamlc -thread -annot -g -package str -package core -I src' --infer src/parser.mly
Warning: 5 states have shift/reduce conflicts.
Warning: 19 shift/reduce conflicts were arbitrarily resolved.
Finished, 58 targets (0 cached) in 00:00:15.
>$ ./synml.native tests/natlist/stutter.ml
let stutter : list -> list =
  let rec f1 (l1:list) : list =
    match l1 with
      | Nil -> []
      | Cons (n1, l2) -> Cons (n1, Cons (n1, f1 l2))
  in
    f1
;;

Test Locations
--------------
tests/pldi-2015-benchmarks
tests/pldi-2015-extended
tests/pldi-2015-contexts

.ml files = test
.out file = baseline output
