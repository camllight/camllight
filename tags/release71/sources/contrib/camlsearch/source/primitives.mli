(* (rev beginning) @ end *)
(* beginning end -> union *)
value unite_rev : 'a list -> 'a list -> 'a list;;

(* Apply `continue' on each part of `list' *)
(* continue list *)
value apply_part : ('a list -> 'a list -> unit) -> 'a list -> unit;;

(* Apply `continue' on each part of `list' of length `n' *)
(* continue list n *)
value apply_part_n :
  ('a list -> 'a list -> unit) -> 'a list -> int -> unit;;

(* Apply `continue' on each element of `list' *)
(* element continue list *)
value apply_element : ('a -> 'a list -> unit) -> 'a list -> unit;;

(* Apply `continue' on each  permutation of `list' *)
(* permutation continue list *)
value apply_permutation : ('a list -> unit) -> 'a list -> unit;;
