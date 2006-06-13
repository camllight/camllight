#open "external_type";;

(* Write more information for interface tools *)
value verbose : bool ref;;

value write_module_name : bool ref;;

(* Convert a global name into a string *)
value string_of_global_name : GLOBAL_NAME -> string;;

(* Convert a type into a string *)
value string_of_type : EXTERNAL_TYPE -> string;;

(* Convert a value into a string *)
value string_of_value :
  (string * GLOBAL_NAME * (int * int * EXTERNAL_TYPE)) -> string;;
