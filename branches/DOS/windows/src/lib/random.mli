(* Pseudo-random number generator *)

value init : int -> unit;;
  (* Initialize the generator, using the argument as a seed.
     The same seed will always yield the same sequence of numbers. *)
value full_init : int vect -> unit;;
  (* Same as [init] but takes more data as seed.  It is not
     useful to give more than 55 integers. *)

value int : int -> int
  (* [random__int bound] returns a random number between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be positive and smaller
     than $2^{30}$. *)
and float : float -> float
  (* [random__float bound] returns a random number between 0 (inclusive)
     and [bound] (exclusive). *)
;;
