#open "general";;
#open "type_lexer";;
#open "type_parser";;

(* Syntax and lexical errors *)
let prerr_location string pos_min pos_max =
  prerr_endline string;
  for i = 0 to pos_min - 1 do
    if (string_length string > i) && (nth_char string i = `\t`) then
      prerr_char `\t`
    else
      prerr_char ` `
  done;
  for i = pos_min to pos_max - 1 do prerr_char `^` done;
  prerr_endline "";;

(* Read a type from a string *)
let type_of_string string =
  let lex_buf = lexing__create_lexer_string string in
    try
      TypeEntry Main lex_buf
    with
      Lexical_error (pos1, pos2) ->
      	prerr_endline "Lexical error :";
      	prerr_location string pos1 pos2;
	raise Error
    | parsing__Parse_error ->
      	prerr_endline "Syntax error";
      	prerr_location 
      	  string
      	  (lexing__get_lexeme_start lex_buf)
          (lexing__get_lexeme_end lex_buf);
      	raise Error;;

(* Usage *)
let usage () =
  prerr_endline "Usage:";
  prerr_endline "camlsearch [-x] [-s] [-nounit]            -e type [.zi files or directories]";
  prerr_endline "camlsearch [-x] [-s] [-nounit] [-d depth] -l type [.zi files or directories]";
  prerr_endline "camlsearch [-x] [-s] [-nounit] [-d depth] -m type [.zi files or directories]";
  prerr_endline "-nounit : do not collapse unit types";
  prerr_endline "-d      : limit depth of search";
  prerr_endline "-e      : search modulo equality";
  prerr_endline "-l      : search for a less general type";
  prerr_endline "-m      : search for a more general type";
  prerr_endline "-s      : do not precise the module name for global names";
  prerr_endline "-x      : give extra information";
  exit 1;;

(* parse the command line *)

let modules = ref ([]: string list);; (* argument pointer *)
let search_type = ref "";;
let mode = ref Undefined;;
let verbose = ref false;;
let full_iso = ref true;;
let max_depth = ref 0;;
let limited_depth = ref false;;
let write_module_name = ref true;;

let read_command_line () =
  let speclist =
    [("-nounit", arg__Unit (function _ -> full_iso := false));
     ("-d", arg__Int
               (function depth -> limited_depth := true; max_depth := depth));
     ("-e", arg__String
      	       (function typ -> search_type := typ; mode := Equality));
     ("-l", arg__String
      	       (function typ -> search_type := typ; mode := Less_general));
     ("-m", arg__String
      	       (function typ -> search_type := typ; mode := More_general));
     ("-s", arg__Unit (function _ -> write_module_name := false));
     ("-x", arg__Unit (function _ -> verbose := true))]
  and anonfun m_name = modules:=m_name::!modules in
    arg__parse speclist anonfun;
    if
      !mode = Undefined
    then usage ();
    if !max_depth < 0 then limited_depth := false;
    try
      let typ = type_of_string !search_type in
        (!mode,
      	 typ,
      	 !modules,
      	 !verbose,
      	 !full_iso,
      	 !limited_depth,
         !max_depth,
      	 !write_module_name)
    with
      Error -> exit 1;;
