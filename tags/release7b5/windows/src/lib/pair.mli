(* Operations on pairs *)

value fst : 'a * 'b -> 'a = 1 "field0"
        (* Return the first component of a pair. *)
  and snd : 'a * 'b -> 'b = 1 "field1"
        (* Return the second component of a pair. *)
  and split : ('a * 'b) list -> 'a list * 'b list
        (* Transform a list of pairs into a pair of lists:
           [split [(a1,b1); ...; (an,bn)]] is [([a1; ...; an], [b1; ...; bn])]
        *)
  and combine : 'a list * 'b list -> ('a * 'b) list
        (* Transform a pair of lists into a list of pairs:
           [combine ([a1; ...; an], [b1; ...; bn])] is
              [[(a1,b1); ...; (an,bn)]].
           Raise [Invalid_argument "combine"] if the two lists
           have different lengths. *)
  and map_combine : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
        (* [map_combine f ([a1; ...; an], [b1; ...; bn])] is
           [[f (a1, b1); ...; f (an, bn)]].
	   Raise [invalid_argument "map_combine"]
	   if the two lists have different lengths. *)
  and do_list_combine : ('a * 'b -> 'c) -> 'a list * 'b list -> unit
        (* [do_list_combine f ([a1; ...; an], [b1; ...; bn])] calls in turn
           [f (a1, b1); ...; f (an, bn)], discarding the results.
	   Raise [Invalid_argument "do_list_combine"] if the two lists have
	   different lengths. *)
;;
