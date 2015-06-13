(* The set of directories to search for includes *)
let include_directories : string list ref = ref ["."]

(* The initial matching depth of the refinement tree *)
let match_count : int ref = ref 0

(* The maximum size of match scrutinees *)
let scrutinee_size_lim : int ref = ref 1

(* The maximum size of E-guessed terms *)
let max_eguess_size : int ref = ref 13

(* Enables caching of generated E-terms *)
let eterm_lookup_tables : bool ref = ref true

(* Enables caching of evaluation results *)
let eval_lookup_tables : bool ref = ref true

(* Enables pretty printed constructors (e.g., nats as numbers) *)
let pretty_ctors : bool ref = ref true

(* Enables timing mode *)
let timing_mode : bool ref = ref false
let print_time_if_timing (f:unit -> 'a) : 'a =
  if !timing_mode then
    let (time, res) = Util.time_action f in
    Printf.printf "time (s) = %.3f\n%!" time;
    res
  else
    f ()

(* If false, then prints out the first occurrence of trying to evaluate an
 * example with incomplete constraints (i.e,. NoMatch failure) *)
let incomplete_constraints_flag : bool ref = ref false

(* Enables verbose mode *)
let verbose_mode : bool ref = ref false
let do_if_verbose (f:unit -> unit) = if !verbose_mode then f () ;;
