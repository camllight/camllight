#open "syntaxe";;
type 'a env;;
value environnement_initial:
      (string * décl_proc) list -> (string * décl_fonc) list -> 'a env
  and ajoute_variable: string -> 'a -> 'a env -> 'a env
  and cherche_variable: string -> 'a env -> 'a
  and cherche_fonction: string -> 'a env -> décl_fonc
  and cherche_procédure: string -> 'a env -> décl_proc;;
exception Pas_trouvé of string;;
