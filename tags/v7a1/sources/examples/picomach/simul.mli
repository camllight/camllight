#open "code";;

exception Erreur of string * int;;

value lire_mémoire : int -> int;;
value écrire_mémoire : int -> int -> unit;;
value lire_registre : int -> int;;
value écrire_registre : int -> int -> unit;;
value tableau_des_appels_système: (int -> int) vect;;

value exécute: instruction vect -> int -> unit;;
