(* Equality functions *)

value prefix = : 'a -> 'a -> bool = 2 "tree_equal"
        (* [e1 = e2] tests for structural equality of [e1] and [e2].
           Mutable structures (e.g. references) are equal if and only if
           their current contents are structurally equal, even if
           the two mutable objects are not the same physical object.
           Equality between functional values raises [Invalid_argument].
           Equality between cyclic data structures may not terminate. *)
  and prefix <> : 'a -> 'a -> bool
        (* Negation of [prefix =]. *)
  and prefix == : 'a -> 'a -> bool = 2 "=="
        (* [e1 == e2] tests for physical equality of [e1] and [e2].
           On integers and characters, it is the same as structural
           equality. On mutable structures, [e1 == e2] is true if and only if
           physical modification of [e1] also affects [e2]. On non-mutable
           structures, the behavior of [prefix ==] is
           implementation-dependent, except that [e1 == e2] implies [e1 = e2]. *)
  and prefix != : 'a -> 'a -> bool = 2 "!="
        (* Negation of [prefix ==]. *)
;;
