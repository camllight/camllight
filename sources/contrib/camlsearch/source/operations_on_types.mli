#open "comparisons";;
#open "type_for_matching";;


(************************** CONVERSION ***********************************)

(* Translate a frozen variable from the subject type to the pattern type *)
value translate_frozen_variable :
  PARB -> FROZEN_VARIABLE -> PARB * FROZEN_VARIABLE;;

(************************** STATE *******************************************)

(* Is  `var' bound ? *)
(* var -> y/n *)
value frozen_var_bound :
  PARB -> FROZEN_VARIABLE * PATTERN_TYPE PARAMETER -> bool;;

(* Return the state of `var' *)
(* var -> state *)
value subst_var_state :
  PARB -> SUBST_VARIABLE * PATTERN_TYPE PARAMETER -> PATTERN_TYPE VALUE;;


(************************** COMPARISONS **********************************)

(* Compare `var1' and `var2' *)
(* var1 var2 -> comp *)
value free_variable_compare :
  int * 'a PARAMETER -> int * 'a PARAMETER -> COMP;;

(* Is `variable1' less or egal to `variable2' *)
(* variable1 variable2 -> comp *)
value free_variable_less : int * 'a -> int * 'a -> bool;;

(* Compare `var1' and `var2' *)
(* var1 var2 -> comp *)
value frozen_var_compare :
  PARB -> FROZEN_VARIABLE * PATTERN_TYPE PARAMETER ->
    FROZEN_VARIABLE * SUBJECT_TYPE PARAMETER -> COMP;;

(* Compare `var1' and `var2' *)
(* var1 var2 -> comp *)
value bound_var_compare :
  PARB -> FROZEN_VARIABLE * PATTERN_TYPE PARAMETER ->
    FROZEN_VARIABLE * PATTERN_TYPE PARAMETER -> COMP;;

(* Compare `const1' and `const2' *)
(* const1 const2 -> comp *)
value pattern_constant_compare :
  P_CONSTANT * PATTERN_TYPE list PARAMETER ->
    P_CONSTANT * PATTERN_TYPE list PARAMETER -> COMP;;

(* Is `const1' less or egal to `const2' *)
(* const1 const2 -> comp *)
value pattern_constant_less :
  P_CONSTANT * 'a -> P_CONSTANT * 'a -> bool;;

(* Compare `const1' and `const2' *)
(* const1 const2 -> comp *)
value subject_constant_compare :
  S_CONSTANT * SUBJECT_TYPE PARAMETER ->
    S_CONSTANT * SUBJECT_TYPE PARAMETER -> COMP;;

(* Is `const1' less or egal to `const2' *)
(* const1 const2 -> comp *)
value subject_constant_less :
  S_CONSTANT * 'a -> S_CONSTANT * 'a -> bool;;


(************************** VARIABLES ASSOCIATION ************************)

(* Associate `var1' and `var2' *)
(* var1 var2 continue *)
value frozen_var_associate :
  PARB -> FROZEN_VARIABLE -> FROZEN_VARIABLE -> PARB;;
(*
(* Associate `var1' and `var2' *)
(* var1 var2 continue *)
value subst_var_associate :
  SUBST_VARIABLE -> SUBJECT_TYPE -> (unit -> unit) -> unit;;
*)

(************************** VARIOUS FUNCTIONS ****************************)

(* Merge two ordered lists of functions *)
value function_merge : ('b * 'c PARAMETER -> 'b * 'c PARAMETER -> COMP) -> ('b * 'c PARAMETER) list -> ('b * 'c PARAMETER) list -> ('b * 'c PARAMETER) list;;

(* Create a new substitution variable *)
value new_subst_variable :
  PARB -> SUBST_VARIABLE -> PARB * SUBST_VARIABLE * int;;

value new_subst_table : int -> SUBTAB_TYPE;;

value merge_pattern_type :
  PATTERN_TYPE -> PATTERN_TYPE -> PATTERN_TYPE;;

value subst_variable_add :
  PARB -> SUBST_VARIABLE -> PATTERN_TYPE -> PARB;;

value change_rest : PARB -> REST_TYPE -> PARB;;

value push_type :
  PARB -> PATTERN_TYPE -> SUBJECT_TYPE -> (PARB -> unit) -> unit;;


(************************** DEPTH ****************************************)

value initialise : unit -> unit;;

value partial_visit : bool ref;;
