(* Association tables over ordered types *)

(* This module implements applicative association tables, also known as
   finite maps or dictionaries, given a total ordering function
   over the keys.
   All operations over maps are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map. *)

type ('a, 'b) t;;
        (* The type of maps from type ['a] to type ['b]. *)

value empty: ('a -> 'a -> int) -> ('a, 'b) t
        (* The empty map.
           The argument is a total ordering function over the set elements.
           This is a two-argument function [f] such that
           [f e1 e2] is zero if the elements [e1] and [e2] are equal,
           [f e1 e2] is strictly negative if [e1] is smaller than [e2],
           and [f e1 e2] is strictly positive if [e1] is greater than [e2].
           Examples: a suitable ordering function for type [int]
           is [prefix -]. For type [string], you could use
           [compare_strings]. *)
  and add: 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
        (* [add x y m] returns a map containing the same bindings as
           [m], plus a binding of [x] to [y]. Previous bindings for [x] 
           in [m] are not removed, but simply hidden: they reappear
           after performing a [remove] operation.
           (This is the semantics of association lists.) *)
  and find:'a -> ('a, 'b) t -> 'b
        (* [find x m] returns the current binding of [x] in [m],
           or raises [Not_found] if no such binding exists. *)
  and remove: 'a -> ('a, 'b) t -> ('a, 'b) t
        (* [remove x m] returns a map containing the same bindings
           as [m] except the current binding for [x]. The previous
           binding for [x] is restored if it exists. [m] is returned
           unchanged if [x] is not bound in [m]. *)
  and iter: ('a -> 'b -> 'c) -> ('a, 'b) t -> unit
        (* [iter f m] applies [f] to all bindings in map [m],
	   discarding the results.
           [f] receives the key as first argument, and the associated value
           as second argument. The order in which the bindings are passed to
           [f] is unspecified. Only current bindings are presented to [f]:
           bindings hidden by more recent bindings are not passed to [f]. *)
;;
