(* Operations on lists *)

value list_length : 'a list -> int
        (* Return the length (number of elements) of the given list. *)
  and prefix @ : 'a list -> 'a list -> 'a list
        (* List concatenation. *)
  and hd : 'a list -> 'a
        (* Return the first element of the given list. Raise
           [Failure "hd"] if the list is empty. *)
  and tl : 'a list -> 'a list
        (* Return the given list without its first element. Raise
           [Failure "tl"] if the list is empty. *)
  and rev : 'a list -> 'a list
        (* List reversal. *)
  and map : ('a -> 'b) -> 'a list -> 'b list
        (* [map f [a1; ...; an]] applies function [f] to [a1, ..., an],
           and builds the list [[f a1; ...; f an]]
           with the results returned by [f]. *)
  and do_list : ('a -> 'b) -> 'a list -> unit
        (* [do_list f [a1; ...; an]] applies function [f] in turn to
           [a1; ...; an], discarding all the results. It is equivalent to
	   [begin f a1; f a2; ...; f an; () end]. *)
  and it_list : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
        (* [it_list f a [b1; ...; bn]] is [f (... (f (f a b1) b2) ...) bn]. *)
  and list_it : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
        (* [list_it f [a1; ...; an] b] is [f a1 (f a2 (... (f an b) ...))]. *)
  and map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
        (* [map2 f [a1; ...; an] [b1; ...; bn]] is [[f a1 b1; ...; f an bn]].
	   Raise [Invalid_argument "map2"] if the two lists have
           different lengths. *)
  and do_list2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> unit
        (* [do_list2 f [a1; ...; an] [b1; ...; bn]] calls in turn
           [f a1 b1; ...; f an bn], discarding the results.
	   Raise [Invalid_argument "do_list2"] if the two lists have
	   different lengths. *)
  and it_list2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
        (* [it_list2 f a [b1; ...; bn] [c1; ...; cn]] is
               [f (... (f (f a b1 c1) b2 c2) ...) bn cn].
	   Raise [Invalid_argument "it_list2"] if the two lists have
	   different lengths. *)
  and list_it2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
        (* [list_it2 f [a1; ...; an] [b1; ...; bn] c] is
               [f a1 b1 (f a2 b2 (... (f an bn c) ...))].
	   Raise [Invalid_argument "list_it2"] if the two lists have
	   different lengths. *)
  and flat_map : ('a -> 'b list) -> 'a list -> 'b list
        (* [flat_map f [l1; ...; ln]] is [(f l1) @ (f l2) @ ... @ (f ln)]. *)
  and for_all : ('a -> bool) -> 'a list -> bool
        (* [for_all p [a1; ...; an]] is [(p a1) & (p a2) & ... & (p an)]. *)
  and exists : ('a -> bool) -> 'a list -> bool
        (* [exists p [a1; ...; an]] is [(p a1) or (p a2) or ... or (p an)]. *)
;;

value mem : 'a -> 'a list -> bool
        (* [mem a l] is true if and only if [a] is structurally equal (see
           module [eq]) to an element of [l]. *)
  and memq : 'a -> 'a list -> bool
        (* [memq a l] is true if and only if [a] is physically equal (see
           module [eq]) to an element of [l]. *)
  and except : 'a -> 'a list -> 'a list
        (* [except a l] returns the list [l] where the first element
           structurally equal to [a] has been removed.
           The list [l] is returned unchanged if it does not contain [a]. *)
  and exceptq : 'a -> 'a list -> 'a list
        (* Same as [except], with physical equality instead of structural
           equality. *)
  and subtract : 'a list -> 'a list -> 'a list
        (* [subtract l1 l2] returns the list [l1] where all elements
           structurally equal to one of the elements of [l2]
           have been removed. *)
  and union : 'a list -> 'a list -> 'a list
        (* [union l1 l2] appends before list [l2] all the elements of list [l1]
           that are not structurally equal to an element of [l2]. *)
  and intersect : 'a list -> 'a list -> 'a list
        (* [intersect l1 l2] returns the list of the elements of [l1] that
           are structurally equal to an element of [l2]. *)
  and index : 'a -> 'a list -> int
        (* [index a l] returns the position of the first element of list [l]
           that is structurally equal to [a]. The head of the list has
           position 0. Raise [Not_found] if [a] is not present in [l]. *)
;;
value assoc : 'a -> ('a * 'b) list -> 'b
        (* [assoc a l] returns the value associated with key [a] in the list of
           pairs [l]. That is,
             [assoc a [ ...; (a,b); ...] = b]
           if [(a,b)] is the leftmost binding of [a] in list [l].
           Raise [Not_found] if there is no value associated with [a] in the
           list [l]. *)
  and assq :  'a -> ('a * 'b) list -> 'b
        (* Same as [assoc], but use physical equality instead of structural
           equality to compare keys. *)
  and mem_assoc : 'a -> ('a * 'b) list -> bool
        (* Same as [assoc], but simply return true if a binding exists,
           and false if no bindings exist for the given key. *)
;;
