type 'a t;;
value vide: 'a t
  and ajoute: 'a t -> int -> 'a -> 'a t
  and extraire: 'a t -> int * 'a * 'a t;;
exception File_vide;;
