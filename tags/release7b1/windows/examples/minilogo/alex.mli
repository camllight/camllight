type lexème =
     Mot of string
   | Symbole of char
   | Constante_entière of int
   | Constante_flottante of float;;

value analyseur_lexical: char stream -> lexème stream;;
