#open "parser";;

type lexical_error =
    Illegal_character
  | Unterminated_comment
  | Bad_char_constant
  | Unterminated_string;;

exception Lexical_error of lexical_error * int * int;;

value main: lexing__lexbuf -> token
  and add_infix: string -> unit
  and remove_infix: string -> unit
;;
