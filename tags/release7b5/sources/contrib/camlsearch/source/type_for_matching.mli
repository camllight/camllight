type PATTERN_TYPE =
  {P_constant : (P_CONSTANT * PATTERN_TYPE list PARAMETER) list;
   P_frozen_variable : (FROZEN_VARIABLE * PATTERN_TYPE PARAMETER) list;
   P_subst_variable : (SUBST_VARIABLE * PATTERN_TYPE PARAMETER) list;
   P_arity : int}

and SUBJECT_TYPE =
  {S_constant : (S_CONSTANT * SUBJECT_TYPE PARAMETER) list;
   S_frozen_variable : (FROZEN_VARIABLE * SUBJECT_TYPE PARAMETER) list;
   S_arity : int}
and P_CONSTANT == string
and S_CONSTANT == string * SUBJECT_TYPE list

and FROZEN_VARIABLE == int
and SUBST_VARIABLE == int
and 'a PARAMETER =
  {Count : int;
         (* ^ list_length Parameter *)
   Parameter : 'a list};;
type 'a VALUE =
    Free
  | Bound of 'a;;
type DEPTH == int;;
type SUBTAB_TYPE == (SUBST_VARIABLE * (DEPTH * PATTERN_TYPE VALUE)) list;;
type REST_TYPE == (PATTERN_TYPE * SUBJECT_TYPE) list;;
type FROTAB_TYPE == (FROZEN_VARIABLE * FROZEN_VARIABLE) list;;
type PARB = 
  {Max_depth : int;
   Depth : int;
   Sub_nb : int;
   Fro_nb : int;
   Sub_tab : SUBTAB_TYPE;
   Fro_tab : FROTAB_TYPE;
   Rest : REST_TYPE};;
value Unit : PATTERN_TYPE;;

value Unit_cost : int;;
