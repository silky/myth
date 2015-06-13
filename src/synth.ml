open Core.Std
open Consts
open Lang
open Rtree

type synth_step =
  | SynthSaturate of float        (* Try to saturate e-guesses up to max_eguess_size *)
  | SynthGrowMatches              (* Increase the depth of matches by 1 *)
  | SynthGrowScrutinees of int    (* Grow the scrutinees of all matches by n *)

type synth_plan = synth_step list

(*
let standard_synth_plan : synth_plan =
  [ SynthSaturate 0.25
  ; SynthGrowMatches
  ; SynthSaturate 0.50
  ; SynthGrowMatches
  ; SynthSaturate 0.75
  ; SynthGrowScrutinees 5
  ; SynthSaturate 1.0
  ; SynthGrowMatches
  ; SynthSaturate 2.0
  ; SynthGrowScrutinees 5
  ; SynthSaturate 5.0
  ]
*)

let standard_synth_plan : synth_plan =
  [ SynthSaturate 0.25
  ; SynthGrowMatches
  ; SynthSaturate 0.25
  ; SynthGrowMatches
  ; SynthSaturate 0.25
  ; SynthGrowScrutinees 5
  ; SynthSaturate 0.25
  ; SynthGrowMatches
  ; SynthSaturate 0.25
  ; SynthGrowScrutinees 5
  ; SynthSaturate 0.25
  ]

let rec saturate_guesses (timeout:float) (s:Sig.t) (env:env) (t:rtree) =
  let rec update n =
    if n <= !max_eguess_size then begin
      update_exps timeout s env t;
      propogate_exps t |> ignore;
      update (n+1)
    end
  in
    update 1

let execute_synth_step (s:Sig.t) (env:env) (t:rtree) (st:synth_step) : exp option =
  reset_timeouts t;
  begin match st with
  | SynthSaturate timeout -> begin
      do_if_verbose (fun _ -> Printf.printf "Saturating E-guesses (timeout = %.2f)...\n%!" timeout);
      saturate_guesses timeout s env t
    end
  | SynthGrowMatches -> begin
      do_if_verbose (fun _ -> Printf.printf "Growing matches...\n%!");
      grow_matches s env t
    end
  | SynthGrowScrutinees k -> begin
      do_if_verbose (fun _ -> Printf.printf "Growing scrutinees by %d...\n%!" k);
      grow_scrutinees s env k t
    end
  end;
  do_if_verbose (fun _ -> Printf.printf "%s\n%!" (Rtree.pp_rtree t));
  let es = propogate_exps t in
  begin match es with
  | [] -> None
  | e :: _ -> Some e
  end

let rec execute_synth_plan (s:Sig.t) (env:env) (t:rtree) (plan:synth_plan) : exp option =
  match plan with
  | [] -> None
  | st :: plan -> begin
    match execute_synth_step s env t st with
    | Some e -> Some e
    | None -> execute_synth_plan s env t plan
    end

let synthesize (s:Sig.t) (env:env) (t:rtree) : exp option =
  execute_synth_plan s env t standard_synth_plan
