(* The lexer generator. Command-line parsing. *)

#open "sys";;
#open "lexing";;
#open "parsing";;
#open "syntax";;
#open "scanner";;
#open "grammar";;
#open "lexgen";;
#open "output";;

let main () =
  if vect_length command_line != 2 then begin
    prerr_endline "Usage: camllex <input file>";
    io__exit 2
  end;
  let source_name = command_line.(1) in
  let dest_name =
    if filename__check_suffix source_name ".mll" then
      filename__chop_suffix source_name ".mll" ^ ".ml"
    else
      source_name ^ ".ml" in
  ic := open_in_bin source_name;
  oc := open_out dest_name;
  let lexbuf =
    create_lexer_channel !ic in
  let (Lexdef(header,_) as def) =
    try
      lexer_definition main lexbuf
    with exn ->
      close_out !oc;
      sys__remove dest_name;
      begin match exn with
        Parse_error ->
          prerr_string "Syntax error around char ";
          prerr_int (get_lexeme_start lexbuf);
          prerr_endline "."
      | scan_aux__Lexical_error s ->
          prerr_string "Lexical error around char ";
          prerr_int (get_lexeme_start lexbuf);
          prerr_string ": ";
          prerr_string s;
          prerr_endline "."
      | _ -> raise exn
      end;
      exit 2 in
  let ((init, states, acts) as dfa) = make_dfa def in
  output_lexdef header dfa;
  close_in !ic;
  close_out !oc
;;

printexc__f main (); exit 0;;
