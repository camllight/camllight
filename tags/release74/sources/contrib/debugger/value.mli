(************************ Values and objects manipulation *******************)

(*** Boxed values and objects (type). ***)
type VALUE;;
type OBJECT;;

(*** Invalid value ***)

(* Return the invalid value used to flag unbound globals *)
value invalid_value : unit -> VALUE
  = 1 "invalid_value";;
(* Is the value valid (global variable bound). *)
value valid_value : VALUE -> bool
  = 1 "valid_value";;

(*** Informations about objects. ***)

value value_size : VALUE -> int;;
value value_tag : VALUE -> int;;
value value_nth_field : VALUE -> int -> VALUE;;

value object_size : OBJECT -> int
  = 1 "object_size";;
value object_tag : OBJECT -> int
  = 1 "object_tag";;
value object_nth_field : OBJECT -> int -> VALUE
  = 2 "object_nth_field";;

(*** Values conversion. ***)

value int_of_value : VALUE -> int
  = 1 "int_of_value";;
value char_of_value : VALUE -> char;;
value bool_of_value : VALUE -> bool;;
value float_of_value : VALUE -> float;;
value string_of_value : VALUE -> string;;

value ecrit : VALUE -> unit = 1 "ecrit";;
