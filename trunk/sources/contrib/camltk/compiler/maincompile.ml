#open "lexer";;
#open "parser";;
#open "tables";;
#open "compile";;


let input_name = ref "Widgets.src"
;;
let include_file = ref "builtins"
;;

let usage () = 
  prerr_string "Usage: tkcompiler input.src\n";
  flush std_err;
  exit 1
;;


let prerr_error_header () =
      prerr_string "File \""; prerr_string !input_name;
      prerr_string "\", line ";
      prerr_string (string_of_int !current_line);
      prerr_string ": "
;;


let parse_file filename =
  let ic = open_in_bin filename in
  try
    let lexbuf = lexing__create_lexer_channel ic in
      while true do
       Entry Main lexbuf
      done
  with
    parsing__Parse_error _ ->
      close_in ic;
      prerr_error_header();
      prerr_string "syntax error \n";
      exit 1
  | lexer__Lexical_error s ->
      close_in ic;
      prerr_error_header();
      prerr_string "lexical error (";
      prerr_string s;
      prerr_string ")\n";
      exit 1
  | Duplicate_Definition (s,s') ->
      close_in ic;
      prerr_error_header();
      prerr_string s; prerr_string " "; prerr_string s';
      prerr_string " is defined twice.\n";
      exit 1
  | End_of_file ->
      close_in ic
;;

let compile () = 
  let oc = open_out_bin "lib/tkgen.ml" in
    let sorted_types = tsort__sort types_order in
    do_list (fun typname ->
      	      try
      	       let typdef = hashtbl__find types_table typname in
      	       	write_type (output_string oc) typname typdef;
      	       	write_CAMLtoTK (output_string oc) typname typdef;
		if mem typname !types_returned then
		  write_TKtoCAML (output_string oc) typname typdef
      	      with 
      	       Not_found -> 
      	       	prerr_string "Type ";
      	       	prerr_string typname;
      	       	prerr_string " is external or undefined\n")
      	    sorted_types;
    do_list (write_function (output_string oc)) !function_table;
    close_out oc;
  hashtbl__do_table 
    (fun wname wdef ->
      let oc = open_out_bin ("lib/" ^ wname ^ ".ml") in
      	output_string oc "#open\"protocol\";;\n";
      	output_string oc "#open\"tk\";;\n";
      	output_string oc "#open\"support\";;\n";
	begin match wdef.ModuleType with
	  Widget ->
            write_create (output_string oc) wname;
      	    do_list (write_command (output_string oc)) wdef.Commands
        | Family ->
	    do_list (write_function (output_string oc)) wdef.Commands
        end;
	close_out oc
     )
   module_table;
  (* write the module list for the Makefile *)
  let oc = open_out_bin "lib/modules" in
    output_string oc "WIDGETOBJS=";
    hashtbl__do_table
       (fun name _ ->
	 output_string oc name;
	 output_string oc ".zo ")
       module_table;
    output_string oc "\n";
    close_out oc
;;

let main () =
  arg__parse ["-i", arg__String (fun s -> include_file := s)]
      	     (fun filename ->
	        input_name := filename)
      	      ;
  parse_file !input_name;
  compile ();
  exit 0
;;

printexc__f main ()
;;
