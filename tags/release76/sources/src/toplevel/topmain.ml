(* The Caml Light toplevel system. Main loop *)

#open "config";;
#open "misc";;
#open "location";;
#open "do_phr";;
#open "compiler";;
#open "format";;

let mainloop() =
  try
    sys__catch_break true;
    let lexbuf = lexing__create_lexer_channel std_in in
    input_lexbuf := lexbuf;
    while true do
      try
        print_string toplevel_input_prompt;
        print_flush ();
        reset_rollback();
        do_toplevel_phrase(parse_impl_phrase lexbuf)
      with End_of_file ->
             io__exit 0
         | Toplevel ->
             flush std_err;
             rollback ()
         | sys__Break ->
             print_string(interntl__translate "Interrupted.\n");
             rollback ()
    done

with sys__Sys_error msg ->
      interntl__eprintf "Input/output error: %s.\n" msg;
      exit 2
   | Zinc s ->
      interntl__eprintf "Internal error: %s.\nPlease report it.\n" s;
      exit 100
;;

printexc__f mainloop (); exit 0;;
