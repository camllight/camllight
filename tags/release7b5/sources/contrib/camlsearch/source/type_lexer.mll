(* The lexer definition *)

{

#open "type_parser";;

(* To report lexical errors *)

exception Lexical_error of int * int;;

}

rule Main = parse
    [` ` `\010` `\013` `\009` `\026` `\012`] +
      { Main lexbuf }
  | [`A`-`Z` `a`-`z` `\192`-`\254`]
    ( `_` ? [`A`-`Z` `a`-`z` `'` `0`-`9` `\192`-`\254`] ) *
      { IDENT (get_lexeme lexbuf) }
  | "'" { QUOTE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "*" { STAR }
  | "," { COMMA }
  | "->" { MINUSGREATER }
  | "_" { UNDERSCORE }
  | "__" { UNDERUNDER }
  | eof { EOF }
  | _
      { raise (Lexical_error (get_lexeme_start lexbuf,
      	       	       	      get_lexeme_end lexbuf)) };;
