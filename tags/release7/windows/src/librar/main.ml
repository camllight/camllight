(* The Caml Light libarian. Command-line parsing. *)

#open "config";;
#open "misc";;

let lib_files = ref ([] : string list)
and lib_name = ref "library.zo";;

let anonymous s =
  lib_files := s :: !lib_files;;

let set_output s =
  lib_name := s
and show_version () =
  version__print_banner(); exit 0
and process_include filename =
  do_list anonymous (readword__from_file filename)
and set_stdlib p =
  path_library := p;
  load_path := [!path_library]
and add_include d =
  load_path := d :: !load_path
and set_language lang =
  interntl__language := lang
;;

let main() =
  try
    load_path := [!path_library];
    arg__parse ["-stdlib", arg__String set_stdlib;
                "-I", arg__String add_include;
                "-o", arg__String set_output;
                "-output", arg__String set_output;
                "-v", arg__Unit show_version;
                "-version", arg__Unit show_version;
                "-files", arg__String process_include;
                "-lang", arg__String set_language;
                "-", arg__String anonymous]
             anonymous;
    librar__make_library (rev !lib_files) !lib_name;
    exit 0
  with Toplevel ->
        exit 2
     | sys__Sys_error msg ->
        interntl__eprintf "Input/output error: %s.\n" msg;
        exit 2
     | Zinc s ->
        interntl__eprintf "Internal error: %s.\nPlease report it.\n" s;
        exit 100
;;

printexc__f main (); exit 0;;
