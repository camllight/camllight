(* The Caml Light toplevel system. Command-line parsing and initializations *)

#open "config";;
#open "misc";;
#open "modules";;
#open "symtable";;
#open "format";;

let anonymous s =
  raise (arg__Bad ("don't know what to do with " ^ s))
and set_stdlib p =
  path_library := p;
  load_path := [!path_library]
and add_include d =
  load_path := d :: !load_path
and open_set set =
  try
    default_used_modules := assoc set default_used_interfaces
  with Not_found ->
    raise (arg__Bad ("unknown module set " ^ set))
and debug_option () =
  toplevel__debug_mode true
and set_language lang =
  interntl__language := lang
;;

let init() =
try
  toplevel := true;
  default_used_modules := assoc "cautious" default_used_interfaces;
  load_path := [!path_library];
  typing__warnings := true;
  arg__parse ["-stdlib", arg__String set_stdlib;
              "-I", arg__String add_include;
              "-include", arg__String add_include;
              "-O", arg__String open_set;
              "-open", arg__String open_set;
              "-g", arg__Unit debug_option;
              "-debug", arg__Unit debug_option;
              "-lang", arg__String set_language]
             anonymous;
  default_used_modules := "toplevel" :: !default_used_modules;
  version__print_banner();
  print_newline();
  let ic = open_in_bin sys__command_line.(0) in
  seek_in ic (in_channel_length ic - 20);
  let size_code = input_binary_int ic in
  let size_data = input_binary_int ic in
  let size_symb = input_binary_int ic in
  let size_debug = input_binary_int ic in
  seek_in ic (in_channel_length ic - 20 - size_debug - size_symb);
  load_linker_tables ic;
  set_c_primitives (meta__available_primitives());
  close_in ic;
  meta__global_data.(16) <- obj__repr true; (* 16: cf ../runtime/globals.h *)
  start_compiling_interface "top"

with Toplevel -> exit 2
   | sys__Sys_error msg ->
      interntl__eprintf "Input/output error: %s.\n" msg;
      exit 2
   | Zinc s ->
      interntl__eprintf "Internal error: %s.\nPlease report it.\n" s;
      exit 100
;;

printexc__f init ();;

