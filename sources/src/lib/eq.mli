(* Generic comparisons *)

value prefix = : 'a -> 'a -> bool = 2 "equal"
        (* [e1 = e2] tests for structural equality of [e1] and [e2].
           Mutable structures (e.g. references and arrays) are equal
           if and only if their current contents are structurally equal,
           even if the two mutable objects are not the same physical object.
           Equality between functional values raises [Invalid_argument].
           Equality between cyclic data structures may not terminate. *)
  and prefix <> : 'a -> 'a -> bool = 2 "notequal"
        (* Negation of [prefix =]. *)
  and prefix < : 'a -> 'a -> bool = 2 "lessthan"
  and prefix <= : 'a -> 'a -> bool = 2 "lessequal"
  and prefix > : 'a -> 'a -> bool = 2 "greaterthan"
  and prefix >= : 'a -> 'a -> bool = 2 "greaterequal"
        (* Structural ordering functions. These functions coincide with
           the usual orderings over integer, string and floating-point
           numbers, and extend them to a total ordering over all types.
           The ordering is compatible with [prefix =]. As in the case
           of [prefix =], mutable structures are compared by contents.
           Comparison between functional values raises [Invalid_argument].
           Comparison between cyclic structures may not terminate. *)
  and compare: 'a -> 'a -> int = 2 "compare"
        (* [compare x y] returns [0] if [x=y], a negative integer if
           [x<y], and a positive integer if [x>y]. The same restrictions
           as for [=] apply. [compare] can be used as the comparison function
           required by the [set] and [map] modules. *)
  and min: 'a -> 'a -> 'a
        (* Return the smaller of the two arguments. *)
  and max: 'a -> 'a -> 'a
        (* Return the greater of the two arguments. *)
  and prefix == : 'a -> 'a -> bool = 2 "=="
        (* [e1 == e2] tests for physical equality of [e1] and [e2].
           On integers and characters, it is the same as structural
           equality. On mutable structures, [e1 == e2] is true if and only if
           physical modification of [e1] also affects [e2].
           On non-mutable structures, the behavior of [prefix ==] is
           implementation-dependent, except that [e1 == e2] implies
           [e1 = e2]. *)
  and prefix != : 'a -> 'a -> bool = 2 "!="
        (* Negation of [prefix ==]. *)
;;
