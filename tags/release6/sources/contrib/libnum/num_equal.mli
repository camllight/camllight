(* Polymorphic equality for numbers *)

value equal : 'a -> 'a -> bool
        (* [equal e1 e2] tests for structural equality of [e1] and [e2].
           Numbers (type [num]) inside [e1] and [e2] are correctly
           compared. (The generic [=] primitive does not work correctly
           over numbers, e.g. [1/2] and [2/4] are not considered equal.)
           On other types, [equal] behaves like [=]. *)
;;
