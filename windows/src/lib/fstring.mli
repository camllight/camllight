(* String operations, without sanity checks *)

(* This module implements the same functions as the [string] module,
   but does not perform bound checks on the arguments of the functions.
   The functions are therefore faster than those in the [string] module,
   but calling these functions with incorrect parameters (that is,
   parameters that would cause the [Invalid_argument] exception to be raised
   by the corresponding functions in the [string] module) can crash the
   program. *)

(*--*)

value string_length : string -> int = 1 "string_length"
;;
value nth_char : string -> int -> char = 2 "get_nth_char"
  and set_nth_char : string -> int -> char -> unit = 3 "set_nth_char"
;;
value prefix ^ : string -> string -> string
  and sub_string : string -> int -> int -> string
;;
value create_string : int -> string = 1 "create_string"
  and make_string : int -> char -> string
;;
value fill_string : string -> int -> int -> char -> unit
    = 4 "fill_string"
  and blit_string : string -> int -> string -> int -> int -> unit
    = 5 "blit_string"
  and replace_string : string -> string -> int -> unit
;;
value eq_string : string -> string -> bool = 2 "=string"
  and neq_string : string -> string -> bool = 2 "<>string"
  and le_string : string -> string -> bool = 2 "<=string"
  and lt_string : string -> string -> bool = 2 "<string"
  and ge_string : string -> string -> bool = 2 ">=string"
  and gt_string : string -> string -> bool = 2 ">string"
  and compare_strings : string -> string -> int = 2 "compare_strings"
;;
value string_for_read : string -> string
;;
