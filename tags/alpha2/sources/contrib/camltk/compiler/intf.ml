(* Write .mli for widgets *)
#open "tables";;
#open "compile";;

let write_create_p W wname =
  W "value create : Widget -> option list -> Widget ;;\n";
  W "             (* [create parent options] creates a new widget. Options\n";
  W "                are restricted to the widget class subset. *)\n"
;;

let write_command_p wname W def =
  W "value "; W def.MLName; W " : Widget";
  begin match def.Arg with
    Unit -> ()       (* no arg *)
  | Product tyl -> 
        do_list (function t -> W " -> "; W (ppMLtype t)) tyl
  | t -> W " -> "; W (ppMLtype t)
  end;
  W " -> ";
  W (ppMLtype def.Result);
  W " ;;\n";
  W "               (* tk command: ."; W wname; W " "; W def.TkName; W " *)\n"
;;

let write_function_p W def =
  W "value "; W def.MLName; W " : ";
  begin match def.Arg with
    Product tyl -> 
        do_list (function t -> W (ppMLtype t); W " -> ") tyl
  | t -> W (ppMLtype t); W " -> "
  end;
  W (ppMLtype def.Result);
  W " ;;\n";
  W "               (* tk function: "; W def.TkName; W " *)\n"
;;
