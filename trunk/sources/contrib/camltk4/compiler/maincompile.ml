#open "lexer";;
#open "parser";;
#open "tables";;
#open "compile";;
#open "intf";;

let input_name = ref "Widgets.src"
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
       entry Main lexbuf
      done
  with
    parsing__Parse_error ->
      close_in ic;
      prerr_error_header();
      prerr_string "Syntax error \n";
      exit 1
  | lexer__Lexical_error s ->
      close_in ic;
      prerr_error_header();
      prerr_string "Lexical error (";
      prerr_string s;
      prerr_string ")\n";
      exit 1
  | Duplicate_Definition (s,s') ->
      close_in ic;
      prerr_error_header();
      prerr_string s; prerr_string " "; prerr_string s';
      prerr_string " is defined twice.\n";
      exit 1
  | Compiler_Error s ->
      close_in ic;
      prerr_error_header();
      prerr_string "Internal error: "; prerr_string s; prerr_string "\n";
      prerr_string "Please report bug\n";
      exit 1
  | End_of_file ->
      close_in ic
;;


(* Bad hack because of collision *)
let rename_module = function 
   "toplevel" -> "toplevelw"
 | s -> s
;;

(* hack to provoke production of cCAMLtoTKoptions_constrs *)
let option_hack oc =
  try
   let typdef = hashtbl__find types_table "options" in
   let hack = {parser_arity = OneToken;
     constructors = 
      map (fun c -> 
      	{ component = Constructor;
      	  ml_name = "C" ^ c.ml_name;
      	  template = begin match c.template with
      	      ListArg (x::_) -> x
	    | _ -> fatal_error "bogus hack"
      	    end;
      	  result = UserDefined "options_constrs";
          safe = true}) 
        typdef.constructors;
     subtypes = [];
     requires_widget_context = false} in
    write_CAMLtoTK (output_string oc) "options_constrs" hack
   with
     Not_found -> ()
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
	        if not (mem_assoc typname !types_external) then begin
       	       	  prerr_string "Type ";
      	       	  prerr_string typname;
      	       	  prerr_string " is undeclared external or undefined\n"
                end)
      	    sorted_types;
    option_hack oc;
    do_list (write_function (output_string oc)) !function_table;
    close_out oc;
  (* Write the interface for public functions *)
  (* this interface is used only for documentation *)
  let oc = open_out_bin "lib/tkgen.mli" in
    do_list (write_function_type (output_string oc)) !function_table;
    close_out oc;
  hashtbl__do_table 
    (fun wname wdef ->
      let modname = rename_module wname in
      let oc = open_out_bin ("lib/" ^ modname ^ ".ml") 
      and oc' = open_out_bin ("lib/" ^ modname ^ ".mli") in
      	begin match wdef.module_type with
	  Widget -> output_string oc' ("(* The "^wname^" widget *)\n")
	| Family -> output_string oc' ("(* The "^wname^" commands  *)\n")
	end;
      	output_string oc "#open\"protocol\";;\n";
      	output_string oc "#open\"tk\";;\n";
      	output_string oc' "#open\"tk\";;\n";
      	output_string oc "#open\"support\";;\n";
      	output_string oc' "#open\"support\";;\n";
      	output_string oc "#open\"textvariable\";;\n";
      	output_string oc' "#open\"textvariable\";;\n";
	begin match wdef.module_type with
	  Widget ->
            write_create (output_string oc) wname;
            write_named_create (output_string oc) wname;
	    write_create_p (output_string oc') wname;
	    write_named_create_p (output_string oc') wname;
	    let cmds = sort_components wdef.commands in
      	    do_list (write_function (output_string oc)) cmds;
	    do_list (write_function_type (output_string oc')) cmds;
	    let exts = sort_components wdef.externals in
	    do_list (write_external (output_string oc)) exts;
	    do_list (write_external_type (output_string oc')) exts
        | Family ->
	    let cmds = sort_components wdef.commands in
	    do_list (write_function (output_string oc)) cmds;
	    do_list (write_function_type (output_string oc')) cmds;
	    let exts = sort_components wdef.externals in
	    do_list (write_external (output_string oc)) exts;
	    do_list (write_external_type (output_string oc')) exts

        end;
	close_out oc;
	close_out oc'
     )
   module_table;
  (* write the module list for the Makefile *)
  (* and hack to death until it works *)
  let oc = open_out_bin "lib/modules" in
    output_string oc "WIDGETOBJS=";
    hashtbl__do_table
       (fun name _ ->
	 output_string oc (rename_module name);
	 output_string oc ".zo ")
       module_table;
    output_string oc "\n";
    hashtbl__do_table
       (fun name _ ->
	 output_string oc (rename_module name);
	 output_string oc ".ml ")
       module_table;
    output_string oc ": tkgen.ml\n\n";
    hashtbl__do_table
       (fun name _ ->
	 output_string oc (rename_module name);
	 output_string oc ".zo : ";
	 output_string oc (rename_module name);
	 output_string oc ".ml\n";
	 output_string oc (rename_module name);
	 output_string oc ".zi : ";
	 output_string oc (rename_module name);
	 output_string oc ".mli\n")
       module_table;
    close_out oc
;;

let main () =
  arg__parse []
      	     (fun filename ->
	        input_name := filename)
      	      ;
  try 
    parse_file !input_name;
    compile ();
    exit 0
  with
    Lexical_error s ->
      prerr_string "Invalid lexical character: ";
      prerr_endline s;
      exit 1
 | Duplicate_Definition (s,s') ->
      prerr_string s; prerr_string " "; prerr_string s';
      prerr_endline " is redefined illegally";
      exit 1
 | Invalid_implicit_constructor c ->
      prerr_string "Constructor ";
      prerr_string c;
      prerr_endline " is used implicitly before defined";
      exit 1
 | tsort__Cyclic ->
      prerr_endline "Cyclic dependency of types";
      exit 1
;;

printexc__f main ()
;;
