(* $Id$ *)

#open "stream";;

type token_type =
  IDENT of string | INT of int | OP of string
| BSLASH | DOT | ELSE | EQUAL | FI | IF | LET | LPAREN | RPAREN | SEMICOL
| THEN
;;

value next_token : char stream -> token_type;;
value reset_lexer : char stream -> unit;;
value token_name : token_type -> string;;
