{

#open "primitives";;
#open "parser";;

}

rule Line =	(* Read a whole line *)
  parse
    [ ^ `\n` ]* `\n`
      { let line =
          get_lexeme lexbuf
        in
          sub_string line 0 (string_length line - 1) }
  | [ ^ `\n` ]*
      { get_lexeme lexbuf }
  | eof
      { raise Exit }
and Argument =	(* Read a raw argument *)
  parse
    [ ^ ` ` `\t` ]+
      { ARGUMENT (get_lexeme lexbuf) }
  | [` ` `\t`]+
      { Argument lexbuf }
  | eof
      { EOL }
  | _
      { raise Parse_error }
and Lexeme =	(* Read a lexeme *)
  parse
    [` ` `\t`] +
      { Lexeme lexbuf }
  | [`A`-`Z` `a`-`z` `\192`-`\214` `\216`-`\246` `\248`-`\255` ]
    ( `_` ? [`A`-`Z` `a`-`z` `\192`-`\214` `\216`-`\246` `\248`-`\255` `'` (*'*) `0`-`9` ] ) *
      { IDENTIFIER (get_lexeme lexbuf) }
  | [`0`-`9`]+
    | `0` [`x` `X`] [`0`-`9` `A`-`F` `a`-`f`]+
    | `0` [`o` `O`] [`0`-`7`]+
    | `0` [`b` `B`] [`0`-`1`]+
      { INTEGER (int_of_string (get_lexeme lexbuf)) }
  | `*`
      { STAR }
  | "-"
      { MINUS }
  | "__"
      { UNDERUNDER }
  | "#"
      { SHARP }
  | "@"
      { AT }
  | "::"
      { COLONCOLON }
  | ","
      { COMMA }
  | "_"
      { UNDERSCORE }
  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | "["
      { LBRACKET }
  | "]"
      { RBRACKET }
  | "{"
      { LBRACE }
  | "}"
      { RBRACE }
  | ";"
      { SEMI }
  | "="
      { EQUAL }
  | eof
      { EOL }
  | _
      { raise Parse_error }
;;
