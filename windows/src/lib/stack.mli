(* Stacks *)

(* This module implements stacks (LIFOs), with in-place modification. *)

type 'a t mutable;;
        (* The type of stacks containing elements of type ['a]. *)

exception Empty;;
        (* Raised when [pop] is applied to an empty stack. *)

value new: unit -> 'a t
        (* Return a new stack, initially empty. *)
  and push: 'a -> 'a t -> unit
        (* [push x s] adds the element [x] at the top of stack [s]. *)
  and pop: 'a t -> 'a
        (* [pop s] removes and returns the topmost element in stack [s],
           or raises [Empty] if the stack is empty. *)
  and clear : 'a t -> unit
        (* Discard all elements from a stack. *)
  and length: 'a t -> int
        (* Return the number of elements in a stack. *)
  and iter: ('a -> 'b) -> 'a t -> unit
        (* [iter f s] applies [f] in turn to all elements of [s], from the
           element at the top of the stack to the element at the
           bottom of the stack. The stack itself is unchanged. *)
;;
