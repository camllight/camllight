(* The Caml Light compiler. Command-line parsing. *)

#open "config";;
#open "misc";;
#open "modules";;
#open "compiler";;

let anonymous s =
  if filename__check_suffix s ".ml" then
    let filename = filename__chop_suffix s ".ml" in
    compile_implementation (filename__basename filename) filename ".ml"
  else if filename__check_suffix s ".mlt" then (* profiler temp files *)
    let filename = filename__chop_suffix s ".mlt" in
    compile_implementation (filename__basename filename) filename ".mlt"
  else if filename__check_suffix s ".mli" then
    let filename = filename__chop_suffix s ".mli" in
    compile_interface (filename__basename filename) filename
  else
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
and show_version () =
  version__print_banner(); flush std_err
and show_types_flag () =
  compiler__verbose := true
and debug_option () =
  event__record_events := true; compiler__write_extended_intf := true
and set_language lang =
  interntl__language := lang
and warnings_option () =
  typing__warnings := true
;;

let main() =
try
  sys__catch_break true;
  default_used_modules := assoc "cautious" default_used_interfaces;
  load_path := [!path_library];
  arg__parse ["-stdlib", arg__String set_stdlib;
              "-I", arg__String add_include;
              "-include", arg__String add_include;
              "-O", arg__String open_set;
              "-open", arg__String open_set;
              "-v", arg__Unit show_version;
              "-version", arg__Unit show_version;
              "-i", arg__Unit show_types_flag;
              "-g", arg__Unit debug_option;
              "-debug", arg__Unit debug_option;
              "-lang", arg__String set_language;
              "-", arg__String anonymous;
              "-W", arg__Unit warnings_option]
             anonymous;
  exit 0
with Toplevel -> exit 2
   | sys__Break -> exit 2
   | sys__Sys_error msg ->
      interntl__eprintf "Input/output error: %s.\n" msg;
      exit 2
   | Zinc s ->
      interntl__eprintf "Internal error: %s.\nPlease report it.\n" s;
      exit 100
;;

printexc__f main (); exit 0;;
