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
  | UNSAFE
;;
value entry :
  (lexing__lexbuf  -> token) -> lexing__lexbuf -> unit;;
