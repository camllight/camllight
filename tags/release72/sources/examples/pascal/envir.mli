#open "syntaxe";;
type 'a env;;
value environnement_initial:
      (string * d�cl_proc) list -> (string * d�cl_fonc) list -> 'a env
  and ajoute_variable: string -> 'a -> 'a env -> 'a env
  and cherche_variable: string -> 'a env -> 'a
  and cherche_fonction: string -> 'a env -> d�cl_fonc
  and cherche_proc�dure: string -> 'a env -> d�cl_proc;;
exception Pas_trouv� of string;;
