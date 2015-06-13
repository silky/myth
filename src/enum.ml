open Core.Std
open Consts
open Lang
open Rtree

let rec saturate_guesses_enum (timeout:float) (s:Sig.t) (env:env) (t:rtree) =
  let rec update n =
    if n <= !max_eguess_size then begin
      do_if_verbose (fun _ -> Printf.printf "Synthesizing at size %d\n%!" n);
      update_exps ~short_circuit:false timeout s env t;
      update (n+1)
    end
  in
    update 1

let rec execute_enum_plan (s:Sig.t) (env:env) (t:rtree) : exp list =
  let rec update n =
    if n <= !max_eguess_size then begin
      do_if_verbose (fun _ -> Printf.printf "Synthesizing at size %d\n%!" n);
      update_exps ~short_circuit:false 1.0 s env t;
      update (n+1)
    end
  in
  update 1;
  propogate_exps ~short_circuit:false t

let enumerate (s:Sig.t) (env:env) (t:rtree) : exp list =
  execute_enum_plan s env t
