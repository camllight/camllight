(* Write .mli for widgets *)
#open "tables";;
#open "compile";;

let write_create_p w wname =
  w "value create : Widget -> option list -> Widget ;;\n";
  w "             (* [create p options] creates a new widget with parent p.\n";
  w "                Options are restricted to the widget class subset,\n";
  w "                and checked dynamically. *)\n"
;;

let write_named_create_p w wname =
  w "value create_named : Widget -> string -> option list -> Widget ;;\n";
  w "             (* [create p name options] creates a new widget with\n";
  w "                parent p and new patch component name.\n";
  w "                Options are restricted to the widget class subset,\n";
  w "                and checked dynamically. *)\n"
;;



let write_function_type w def =
  w "value "; w def.MLName; w " : ";
  begin match types_of_template def.Template with
    [] -> w "unit ->"
  | l -> do_list (function t -> w (ppMLtype t);  w " -> ") l
  end;
  w (ppMLtype def.Result);
  w " ;;\n";
  w "(* tk invocation: "; w (doc_of_template def.Template);
  w " *)\n\n";
;;
