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

let write_command_p wname w def =
  w "value "; w def.MLName; w " : Widget";
  begin match def.Arg with
    Unit -> ()       (* no arg *)
  | Product tyl -> 
        do_list (function t -> w " -> "; w (ppMLtype t)) tyl
  | t -> w " -> "; w (ppMLtype t)
  end;
  w " -> ";
  w (ppMLtype def.Result);
  w " ;;\n";
  w "               (* tk command: ."; w wname; w " "; w def.TkName; w " *)\n"
;;

let write_function_p w def =
  w "value "; w def.MLName; w " : ";
  begin match def.Arg with
    Product tyl -> 
        do_list (function t -> w (ppMLtype t); w " -> ") tyl
  | t -> w (ppMLtype t); w " -> "
  end;
  w (ppMLtype def.Result);
  w " ;;\n";
  w "               (* tk function: "; w def.TkName; w " *)\n"
;;
