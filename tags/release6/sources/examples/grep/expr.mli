type expr =
    Epsilon
  | Caract�res of char list
  | Alternative of expr * expr
  | S�quence of expr * expr
  | R�p�tition of expr;;

value lire : char stream -> expr;;
