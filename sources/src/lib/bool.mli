(* Boolean operations *)

(* The boolean and is written [e1 & e2].
   The boolean or  is written [e1 or e2].
   Both constructs are sequential, left-to-right: [e2] is evaluated only
   if needed. Actually,
     [e1 & e2]  is equivalent to  [if e1 then e2 else false],
   and
     [e1 or e2] is equivalent to  [if e1 then true else e2].
*)

value prefix not : bool -> bool = 1 "not"
        (* The boolean negation. *)
;;
