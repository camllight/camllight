#open "type_for_matching";;

(******************** MATCHING ON BASE *************************)
value match_base : (PARB -> unit) -> PARB -> 
                   (SUBST_VARIABLE * PATTERN_TYPE PARAMETER) list -> 
                   (S_CONSTANT * SUBJECT_TYPE PARAMETER) list -> 
                   (FROZEN_VARIABLE * SUBJECT_TYPE PARAMETER) list -> unit;;
