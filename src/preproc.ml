(* NOTE: no Core used here... how do you do file IO in core!? *)

open Consts

let open_file_with_include_dirs (filename:string) : in_channel =
  let rec try_open is =
    match is with
    | [] -> raise @@ Sys_error (Printf.sprintf "File not found: %s" filename)
    | i :: is ->
      let full_path = Filename.concat i filename in
      if Sys.file_exists full_path then
        open_in full_path
      else
        try_open is
  in
    try_open !include_directories

let preproc_re = Str.regexp "^#use \"\\([-a-zA-Z0-9_\\./]+\\)\""

let read_to_string (filename:string) : string =
  let ic   = open_file_with_include_dirs filename in
  let data = ref [] in
  begin try
    while true do
      data := input_line ic :: !data
    done
  with End_of_file ->
    close_in ic
  end;
  List.rev !data |> String.concat "\n"

let add_base_dir_to_includes (path:string) =
  let basedir = Filename.dirname path in
  include_directories := basedir :: !include_directories

let preprocess_file (filename:string) : string =
  add_base_dir_to_includes filename;
  let ic   = open_file_with_include_dirs filename in
  let data = ref [] in
  begin try
    while true do
      let line = input_line ic in
      if Str.string_match preproc_re line 0 then
        let path = Str.matched_group 1 line in
        data := read_to_string path :: !data
      else
        data := line :: !data
    done
  with End_of_file ->
    close_in ic
  end;
  List.rev !data |> String.concat "\n"
