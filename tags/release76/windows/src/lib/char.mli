(* Character operations *)

value int_of_char : char -> int = 1 "identity"
        (* Return the ASCII code of the argument. *)
  and char_of_int : int -> char
        (* Return the character with the given ASCII code.
           Raise [Invalid_argument "char_of_int"] if the argument is
           outside the range 0--255. *)
  and string_of_char : char -> string
        (* Return a string representing the given character. *)
  and char_for_read : char -> string
        (* Return a string representing the given character,
           with special characters escaped following the lexical conventions
           of Caml Light. *)

;;

(*--*)

value is_printable : char -> bool = 1 "is_printable";;
