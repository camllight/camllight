(* Sets over ordered types *)

(* This module implements the set data structure, given a total ordering
   function over the set elements. All operations over sets
   are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and is therefore
   reasonably efficient: insertion and membership take time
   logarithmic in the size of the set, for instance. *)

type 'a t;;
        (* The type of sets containing elements of type ['a]. *)

value empty: ('a -> 'a -> int) -> 'a t
        (* The empty set.
           The argument is a total ordering function over the set elements.
           This is a two-argument function [f] such that
           [f e1 e2] is zero if the elements [e1] and [e2] are equal,
           [f e1 e2] is strictly negative if [e1] is smaller than [e2],
           and [f e1 e2] is strictly positive if [e1] is greater than [e2].
           Examples: a suitable ordering function for type [int]
           is [prefix -]. For type [string], you could use
           [compare_strings]. *)
  and is_empty: 'a t -> bool
        (* Test whether a set is empty or not. *)
  and mem: 'a -> 'a t -> bool
        (* [mem x s] tests whether [x] belongs to the set [s]. *)
  and add: 'a -> 'a t -> 'a t
        (* [add x s] returns a set containing all elements of [s],
           plus [x]. If [x] was already in [s], [s] is returned unchanged. *)
  and remove: 'a -> 'a t -> 'a t
        (* [remove x s] returns a set containing all elements of [s],
           except [x]. If [x] was not in [s], [s] is returned unchanged. *)
  and union: 'a t -> 'a t -> 'a t
  and inter: 'a t -> 'a t -> 'a t
  and diff: 'a t -> 'a t -> 'a t
        (* Union, intersection and set difference. *)
  and equal: 'a t -> 'a t -> bool
        (* [equal s1 s2] tests whether the sets [s1] and [s2] are
           equal, that is, contain the same elements. *)
  and compare: 'a t -> 'a t -> int
        (* Total ordering between sets. Can be used as the ordering function
           for doing sets of sets. *)
  and elements: 'a t -> 'a list
        (* Return the list of all elements of the given set.
           The elements appear in the list in some non-specified order. *)
  and iter: ('a -> 'b) -> 'a t -> unit
        (* [iter f s] applies [f] in turn to all elements of [s], and
           discards the results. The elements of [s] are presented to [f]
           in a non-specified order. *)
  and fold: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        (* [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
           where [x1 ... xN] are the elements of [s].
           The order in which elements of [s] are presented to [f] is
           not specified. *)
;;
