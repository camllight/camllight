(* Result of a comparison *)
type COMP = Inf | Sup | Equ;;


(* Compare two strings *)
value string_compare : string -> string -> COMP;;

(* Compare two integers *)
value int_compare : int -> int -> COMP;;

(* Compare two lists *)
value list_compare : ('a -> 'b -> COMP) -> 'a list -> 'b list -> COMP;;
