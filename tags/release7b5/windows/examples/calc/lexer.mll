{
#open "parser";;        (* The type token is defined in parser.mli *)
exception Eof;;
}
rule Token = parse
    [` ` `\t`]     { Token lexbuf }     (* skip blanks *)
  | [`\n` ]        { EOL }
  | [`0`-`9`]+     { INT(int_of_string (get_lexeme lexbuf)) }
  | `+`            { PLUS }
  | `-`            { MINUS }
  | `*`            { TIMES }
  | `/`            { DIV }
  | `(`            { LPAREN }
  | `)`            { RPAREN }
  | eof            { raise Eof }
;;
