#open "code";;

exception Erreur of string * int;;

value lire_m�moire : int -> int;;
value �crire_m�moire : int -> int -> unit;;
value lire_registre : int -> int;;
value �crire_registre : int -> int -> unit;;
value tableau_des_appels_syst�me: (int -> int) vect;;

value ex�cute: instruction vect -> int -> unit;;
