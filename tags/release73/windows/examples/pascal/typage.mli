#open "syntaxe";;

type erreur_de_type =
    Ind�fini of string      (* variable utilis�e mais non d�finie *)
  | Conflit of string * expr_type * expr_type (* conflit de types *)
  | Arit� of string * int * int     (* mauvais nombre d'arguments *)
  | Tableau_attendu             (* [..] appliqu� � un non-tableau *)
  | Tableau_interdit of string;;   (* tableau renvoy� en r�sultat *)

exception Erreur_typage of erreur_de_type;;

value type_programme: programme -> unit
  and affiche_erreur: erreur_de_type -> unit
  and type_op_unaire: string -> expr_type * expr_type
  and type_op_binaire: string -> expr_type * expr_type * expr_type;;
