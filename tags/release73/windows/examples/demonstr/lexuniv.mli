type lexème =
     MC of string
   | Ident of string
   | Entier of int;;
value construire_analyseur:
     string list -> (char stream -> lexème stream);;
