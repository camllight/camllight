#open "lexing";;

let do_file name =
  let ic = open_in name in
  let lexbuf = create_lexer_channel ic in
  scanmsgs__interntl_is_open := false;
  scanmsgs__main lexbuf;
  close_in ic
;;

let main () =
  for i = 1 to vect_length sys__command_line - 1 do
    do_file sys__command_line.(i)
  done;
  exit 0
;;

printexc__f main ();;
