#open "external_type";;

(* Read a type from a string *)
value type_of_string : string -> int * int * EXTERNAL_TYPE;;

type MODE = Equality | More_general | Less_general | Undefined;;

value read_command_line :
  unit ->
    MODE * (int * int * EXTERNAL_TYPE) * string list * bool * bool * bool * 
      int * bool;;
