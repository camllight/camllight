type lex�me =
     MC of string
   | Ident of string
   | Entier of int;;
value construire_analyseur:
     string list -> (char stream -> lex�me stream);;
