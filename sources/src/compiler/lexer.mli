#open "parser";;

exception Lexical_error of string * int * int
;;

value Main: lexing__lexbuf -> token
  and add_infix: string -> unit
  and remove_infix: string -> unit
;;
