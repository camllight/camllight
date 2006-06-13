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

(* Stuff for the tags *)
value input_chan : in_channel ref
and current_line : int ref
and current_beg : int ref
and last_line : int ref
and last_beg : int ref
and init_mem : in_channel -> unit
;;
