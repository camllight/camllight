type token =
    IDENT of string
  | STRING of string
  | EOF
  | LPAREN
  | RPAREN
  | COMMA
  | SEMICOLON
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | TYINT
  | TYFLOAT
  | TYBOOL
  | TYCHAR
  | TYSTRING
  | LIST
  | WIDGET
  | OPTION
  | TYPE
  | SEQUENCE
  | SUBTYPE
  | FUNCTION
  | MODULE
  | EXTERNAL
;;
value Entry :
  (lexing__lexbuf  -> token) -> lexing__lexbuf -> unit;;
