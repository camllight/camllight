(* Boolean operations *)

value prefix &  : bool -> bool -> bool = 2 "sequand";;
value prefix && : bool -> bool -> bool = 2 "sequand";;
value prefix or : bool -> bool -> bool = 2 "sequor";;
value prefix || : bool -> bool -> bool = 2 "sequor";;
        (* The boolean and is written [e1 & e2] or [e1 && e2].
           The boolean or  is written [e1 or e2] or [e1 || e2].
           Both constructs are sequential, left-to-right:
           [e2] is evaluated only if needed. Actually,
           [e1 & e2]  is equivalent to  [if e1 then e2 else false],
           and
           [e1 or e2] is equivalent to  [if e1 then true else e2].
*)
value prefix not : bool -> bool = 1 "not"
        (* The boolean negation. *)
;;

value string_of_bool : bool -> string
        (* Return a string representing the given boolean. *)
;;
value bool_of_string : string -> bool
        (* Return a boolean representing the given string.
           Raise [Invalid_argument "bool_of_string"] if the given
           string is not ["true"] or ["false"]. *)
;;
