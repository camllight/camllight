type expr =
    Epsilon
  | Caractères of char list
  | Alternative of expr * expr
  | Séquence of expr * expr
  | Répétition of expr;;

value lire : char stream -> expr;;
