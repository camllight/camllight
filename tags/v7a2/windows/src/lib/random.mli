(* Pseudo-random number generator *)

value init : int -> unit;;
  (* Initialize the generator, using the argument as a seed.
     The same seed will always yield the same sequence of numbers. *)

value int : int -> int
  (* [random__int bound] returns a random number between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be smaller than $2^{30}$. *)
and float : float -> float
  (* [random__float] returns a random number between 0 (inclusive)
     and [bound] (exclusive). *)
;;
