(* The Caml Light toplevel system. Command-line parsing and main loop *)

#open "config";;
#open "misc";;
#open "modules";;
#open "sys";;
#open "symtable";;
#open "location";;
#open "do_phr";;
#open "compiler";;

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
;;

let main() =
try
  toplevel := true;
  default_used_modules := assoc "cautious" default_used_interfaces;
  load_path := [!path_library];
  arg__parse ["-stdlib", arg__String set_stdlib;
              "-I", arg__String add_include;
              "-include", arg__String add_include;
              "-O", arg__String open_set;
              "-open", arg__String open_set;
              "-g", arg__Unit debug_option;
              "-debug", arg__Unit debug_option]
             anonymous;
  default_used_modules := "toplevel" :: !default_used_modules;
  print_string version__banner;
  print_newline(); print_newline();
  let ic = open_in_bin command_line.(0) in
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
    start_compiling_interface "top";
    catch_break true;
    let lexbuf = lexing__create_lexer_channel std_in in
    input_lexbuf := lexbuf;
    while true do
      try
        print_string toplevel_input_prompt;
        flush std_out;
        reset_rollback();
        do_toplevel_phrase(parse_impl_phrase lexbuf)
      with End_of_file ->
             io__exit 0
         | Toplevel ->
             flush std_out;
             rollback ()
         | Break ->
             print_begline "Interrupted."; print_endline "";
             flush std_out;
             rollback ()
    done

with Toplevel -> exit 2
   | Zinc s -> prerr_string "# Internal error: "; prerr_endline s; exit 4
;;

printexc__f main ()
;;
