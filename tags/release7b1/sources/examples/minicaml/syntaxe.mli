type expression =
    Variable of string
  | Fonction of (motif * expression) list
  | Application of expression * expression
  | Let of d�finition * expression
  | Bool�en of bool
  | Nombre of int
  | Paire of expression * expression
  | Nil
  | Cons of expression * expression

and motif =
    Motif_variable of string
  | Motif_bool�en of bool
  | Motif_nombre of int
  | Motif_paire of motif * motif
  | Motif_nil
  | Motif_cons of motif * motif

and d�finition =
  { R�cursive: bool;
    Nom: string;
    Expr: expression };;
type phrase =
    Expression of expression
  | D�finition of d�finition;;

value lire_phrase: char stream -> phrase;;
