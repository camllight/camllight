#open "code";;

exception Erreur of string;;

value initialise: unit -> unit
  and assemble: instruction -> unit
  and poser_étiquette: string -> unit
  and valeur_étiquette: string -> int
  and extraire_code: unit -> instruction vect;;
