(* Character operations, without sanity checks *)

(* This module implements the same functions as the [char] module,
   but does not perform bound checks on the arguments of the functions.
   The functions are therefore faster than those in the [char] module,
   but calling these functions with incorrect parameters (that is,
   parameters that would cause the [Invalid_argument] exception to be raised
   by the corresponding functions in the [char] module) can crash the
   program. *)

(*--*)

value int_of_char : char -> int = 1 "identity"
  and char_of_int : int -> char = 1 "identity"
  and char_for_read : char -> string
  and is_printable : char -> bool = 1 "is_printable"
;;
