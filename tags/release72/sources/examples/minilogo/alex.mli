type lex�me =
     Mot of string
   | Symbole of char
   | Constante_enti�re of int
   | Constante_flottante of float;;

value analyseur_lexical: char stream -> lex�me stream;;
