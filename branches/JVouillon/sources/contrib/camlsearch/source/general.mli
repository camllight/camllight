(* Fatal error *)
exception Error;;

value list_filter : ('a -> bool) -> 'a list -> 'a list;;

(* Split `list' in `list1' and `list2' according to `predicate' *)
(* predicate list -> list1 list2 *)
value list_split : ('a -> bool) -> 'a list -> 'a list * 'a list;;
