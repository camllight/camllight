type token =
    IDENT of string
  | INFIX of string
  | INT of int
  | CHAR of char
  | FLOAT of float
  | STRING of string
  | EOF
  | MULTIPLICATIVE of string
  | ADDITIVE of string
  | SUBTRACTIVE of string
  | CONCATENATION of string
  | COMPARISON of string
  | EQUAL
  | EQUALEQUAL
  | SHARP
  | BANG
  | AMPERSAND
  | QUOTE
  | LPAREN
  | RPAREN
  | STAR
  | COMMA
  | MINUSGREATER
  | DOT
  | DOTDOT
  | DOTLPAREN
  | COLON
  | COLONCOLON
  | COLONEQUAL
  | SEMI
  | SEMISEMI
  | LESSMINUS
  | LBRACKET
  | LBRACKETBAR
  | LBRACKETLESS
  | RBRACKET
  | UNDERSCORE
  | UNDERUNDER
  | LBRACE
  | BAR
  | BARRBRACKET
  | GREATERRBRACKET
  | RBRACE
  | AND
  | AS
  | BEGIN
  | DO
  | DONE
  | DOWNTO
  | ELSE
  | END
  | EXCEPTION
  | FOR
  | FUN
  | FUNCTION
  | IF
  | IN
  | LET
  | MATCH
  | MUTABLE
  | NOT
  | OF
  | OR
  | PREFIX
  | REC
  | THEN
  | TO
  | TRY
  | TYPE
  | VALUE
  | WHERE
  | WHILE
  | WITH
;;
value TypeEntry :
  (lexing__lexbuf  -> token) -> lexing__lexbuf -> syntax__type_expression;;
