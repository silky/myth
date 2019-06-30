open Core
open Consts
open Lang

exception Arg_exception

type driver_mode =
  | Default
  | Parse
  | Typecheck
  | Graph
  | Enum
  | Data
  | Synth

let usage_msg = "synml [-help | opts...] <src>"
let filename : string option ref = ref None
let mode : driver_mode ref = ref Default

let set_opt (d:driver_mode) =
  match !mode with
  | Default -> mode := d
  | _ -> raise Arg_exception

let args =
  [ ("-I", Arg.String (fun path -> include_directories := path :: !include_directories)
    , " Search the given directory for included files")
  ; ("-parse", Arg.Unit (fun _ -> set_opt Parse), " Parse only")
  ; ("-typecheck", Arg.Unit (fun _ -> set_opt Typecheck), " Typecheck only")
  ; ("-enum", Arg.Unit (fun _ -> set_opt Enum), " Typecheck and enumerate")
  ; ("-synth", Arg.Unit (fun _ -> set_opt Synth), " Typecheck and synthesize (first found)")
  ; ("-data", Arg.Unit (fun _ -> set_opt Data),
      " Typecheck, synthesize, and collect statistics (name, #/examples, size, time (s))")
  ; ("-time", Arg.Unit (fun _ -> timing_mode := true), " Prints the time to completion")
  ; ("-graph", Arg.Unit (fun _ -> set_opt Graph), " Print split graph")
  ; ("-matches"
    , Arg.Int (fun n -> match_count := n)
    , Printf.sprintf " Sets the starting number of matches synthesized (default: %d)" !match_count)
  ; ( "-scrutinee"
    , Arg.Int (fun n -> scrutinee_size_lim := n)
    , " Sets the maximize size of scrutinees in matches (default: 1)"
    )
  ; ("-nosugar", Arg.Unit (fun n -> pretty_ctors := false), " Disables sugaring of nat and list literals")
  ; ( "-nocache"
    , Arg.Unit (fun _ -> eterm_lookup_tables := false; eval_lookup_tables := false)
    , " Disables caching of generated terms")
  ; ( "-noincomplete-warning"
    , Arg.Unit (fun _ -> incomplete_constraints_flag := true)
    , " Disables incomplete constraints warning")
  ; ( "-verbose"
    , Arg.Unit (fun _ -> verbose_mode := true)
    , " Increases debugging spew"
    )
  ]
  |> Arg.align

let parse_file (f:string) : prog =
  Preproc.preprocess_file f
    |> Lexing.from_string
    |> Parser.prog Lexer.token

let typecheck_prog (p:prog) : prog = let _ = Tc.tc_prog p in p

let process_preamble ((ds, p):prog) =
  let (s, g)      = Tc.tc_prog (ds, p) in
  let env         = Eval.gen_init_env ds in
  let (x, t, es)  = p in
  let vs          = Eval.gen_init_evidence env es in
  let tree        = Rtree.create_rtree s g env t vs !match_count in
  (s, g, env, x, t, es, vs, tree)

let print_graph (p:prog) : prog =
  let (s, g, env, x, t, es, vs, tree) = process_preamble p in
  Printf.printf "%s\nsize = %d\n" (Rtree.pp_rtree tree) (Rtree.rtree_size tree);
  p

let enumerate_progs (p:prog) : prog =
  let (s, g, env, x, t, es, vs, tree) = process_preamble p in
  let es = Enum.enumerate s env tree in
  Printf.printf "%d programs found!\n" (List.length es);
  List.iter ~f:(fun e -> Printf.printf "%s\n" (Translate.to_top_level x t e |> Pp.pp_decl)) es;
  p

let collect_data (p:prog) : prog =
  let (time, (x, vs, e)) = Util.time_action (fun _ ->
    let (s, g, env, x, t, es, vs, tree) = process_preamble p in
    (x, List.map ~f:snd vs, Synth.synthesize s env tree))
  in
    begin match e with
    | Some e ->
      Printf.printf "%s,%d,%d,%.3f\n%!"
        x (List.fold_left ~f:(fun n v -> n + examples_count v) ~init:0 vs)
        (size e) time
    | None ->
      Printf.printf "<<< %s: error during synthesis >>>\n%!" x
    end; p

let synthesize_prog (p:prog) : prog =
  let (s, g, env, x, t, es, vs, tree) = process_preamble p in
  begin match Synth.synthesize s env tree with
  | Some e ->
    Printf.printf "%s\n" (Translate.to_top_level x t e |> Pp.pp_decl)
  | None -> begin
      Printf.printf "No expression found!\n";
      Printf.printf "final rtree size = %d\n" (Rtree.rtree_size tree)
    end
  end;
  p

let main () =
  Gc.tune
    ~minor_heap_size:(1000448 * 32)
    ~major_heap_increment:(1000448 * 64)
    ~space_overhead:200
    (* ~verbose:0x01 *)
      ();

  begin try
    Arg.parse args (fun s ->
      match !filename with
      | Some f -> raise Arg_exception
      | None -> filename := Some s) usage_msg
  with
    Arg_exception -> Arg.usage args usage_msg
  end;
  match !filename with
  | None   -> Arg.usage args usage_msg
  | Some f ->
    print_time_if_timing begin fun _ ->
      begin match Sys.file_exists f with
      | `No | `Unknown -> Arg.usage args ("File not found: " ^ f)
      | `Yes -> begin match !mode with
        | Parse -> let prog = parse_file f in Printf.printf "%s\n" (Pp.pp_prog prog)
        | Typecheck -> parse_file f |> typecheck_prog |> ignore
        | Graph -> parse_file f |> typecheck_prog |> print_graph |> ignore
        | Enum -> parse_file f |> typecheck_prog |> enumerate_progs |> ignore
        | Data -> parse_file f |> typecheck_prog |> collect_data |> ignore
        | Default | Synth -> parse_file f |> typecheck_prog |> synthesize_prog |> ignore
        end
      end
    end

let () = if not !Sys.interactive then main ()
