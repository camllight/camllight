(* Internals of the str library. *)

#open "str";;

value compile_regexp: string -> bool -> regexp = 2 "str_compile_regexp"
  and beginning_group: int -> int = 1 "str_beginning_group"
  and end_group: int -> int = 1 "str_end_group"
  and replacement_text: string -> string -> string = 2 "str_replacement_text"
;;
