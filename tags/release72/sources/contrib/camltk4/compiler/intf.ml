(* Write .mli for widgets *)
#open "tables";;
#open "compile";;

let write_create_p w wname =
  w "value create : widget -> options list -> widget ;;\n";
  w "             (* [create p options] creates a new widget with parent p.\n";
  w "                Options are restricted to the widget class subset,\n";
  w "                and checked dynamically. *)\n"
;;

let write_named_create_p w wname =
  w "value create_named : widget -> string -> options list -> widget ;;\n";
  w "             (* [create p name options] creates a new widget with\n";
  w "                parent p and new patch component name.\n";
  w "                Options are restricted to the widget class subset,\n";
  w "                and checked dynamically. *)\n"
;;



let write_function_type w def =
  if not def.safe then w "(* unsafe *)\n";
  w "value "; w def.ml_name; w " : ";
  begin match types_of_template def.template with
    [] -> w "unit ->"
  | l -> do_list (function t -> w (ppMLtype t);  w " -> ") l
  end;
  w (ppMLtype def.result);
  w " ;;\n";
  w "(* tk invocation: "; w (doc_of_template def.template); w " *)\n";
  if def.safe then w "\n\n"
  else w "\n(* /unsafe *)\n\n"

;;

let write_external_type w def =
  match def.template with
    StringArg fname ->
      let ic = open_in_bin (fname ^ ".mli") in
        if not def.safe then w "(* unsafe *)\n";
      	begin try
	 while true do
	   w (input_line ic);
	   w "\n"
	 done
        with
	 End_of_file -> 
      	   close_in ic;
	   if def.safe then w "\n\n"
	   else w "\n(* /unsafe *)\n\n"
        end
  | _ -> raise (Compiler_Error "invalid external definition")
;;
