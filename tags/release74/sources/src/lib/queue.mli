(* Queues *)

(* This module implements queues (FIFOs), with in-place modification. *)

type 'a t;;
        (* The type of queues containing elements of type ['a]. *)

exception Empty;;
        (* Raised when [take] is applied to an empty queue. *)

value new: unit -> 'a t
        (* Return a new queue, initially empty. *)
  and add: 'a -> 'a t -> unit
        (* [add x q] adds the element [x] at the end of the queue [q]. *)
  and take: 'a t -> 'a
        (* [take q] removes and returns the first element in queue [q],
           or raises [Empty] if the queue is empty. *)
  and peek: 'a t -> 'a
        (* [peek q] returns the first element in queue [q], without removing
           it from the queue, or raises [Empty] if the queue is empty. *)
  and clear : 'a t -> unit
        (* Discard all elements from a queue. *)
  and length: 'a t -> int
        (* Return the number of elements in a queue. *)
  and iter: ('a -> unit) -> 'a t -> unit
        (* [iter f q] applies [f] in turn to all elements of [q], from the
           least recently entered to the most recently entered.
           The queue itself is unchanged. *)
;;
