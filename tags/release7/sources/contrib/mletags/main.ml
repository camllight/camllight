#open "arg";;
#open "sys";;
#open "filename";;
#open "tags";;
#open "lexer";;
#open "parser";;


(* Parsing functions *)

let parse_phrase parsing_fun lexing_fun lexbuf =
  try
    parsing_fun lexing_fun lexbuf
  with parsing__Parse_error ->
         failwith "parser"
     | lexer__Lexical_error(msg, pos1, pos2) ->
         failwith "lexer"
;;

let parse_impl_phrase = parse_phrase Implementation main
and parse_intf_phrase = parse_phrase Interface main
;;



(* TAGS file format for gnu-emacs *)

let outchan = ref stdout;;
let print_string s = output_string !outchan s
and print_char  c   = output_char !outchan c
and print_int  i   = output_string !outchan (string_of_int i)
and print_newline () = output_char !outchan `\n`; flush !outchan
;;

let output_tags filename tags = 
  print_string "\012\n";
  print_string filename; print_string ","; 
  print_int(it_list (fun x (Tag(s,i,l)) ->
                       x + string_length s + string_length(string_of_int i)
                         + string_length(string_of_int l) + 3)
                    0 tags);
  print_newline();
  do_list (function Tag(s,l,i) -> print_string s; 
                                  print_char `\127`;
                                  print_int l;
                                  print_char `,`;
                                  print_int i;
                                  print_newline())
           tags
;;

let tags_ml filename parser = 
  let ic = open_in filename in
    lexer__init_mem ic;
    let lexbuf = lexing__create_lexer_channel ic in
    let tags = ref [] in 
      try 
        while true do
          tags :=  (parser lexbuf) @ !tags
        done
      with End_of_file -> close_in ic; output_tags filename !tags
       | Failure "parser" -> 
         prerr_string filename;
         prerr_string ": error line ";
         prerr_int !(lexer__current_line );
         prerr_endline " (syntax)"
       | Failure "lexer" ->
         prerr_string filename;
         prerr_string "error line ";
         prerr_int !(lexer__current_line );
         prerr_endline " (lexical)"
;;  

let parse_file filename = 
  if check_suffix filename ".mli" then tags_ml filename parse_intf_phrase
  else if check_suffix filename ".ml" then tags_ml filename parse_impl_phrase
  else prerr_endline ("don't know what to do with " ^ filename)
;;

(* Writes to TAGS file *)
let main () =
  let aflag = ref false
  and files = ref ([] : string list) 
  and tagfile = ref "TAGS" in
    parse ["-a", Unit (fun () -> aflag := true); 
           "-o", String (fun s -> tagfile := s)  ]
          (fun s -> files := s::!files);
    outchan := open_out_gen ([O_WRONLY; O_BINARY] @ (if !aflag 
                                                  then [O_APPEND]
                                                  else [O_CREAT; O_TRUNC]))
                            (s_irall + s_iwusr) 
                            !tagfile ;
    do_list parse_file (rev !files);
    close_out !outchan
;;

printexc__f main ();;
