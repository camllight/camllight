type action =
    Print of string
  | Print_arg
  | Skip_arg
  | Read_arg of (string -> unit);;

value find_macro: string -> action list;;
