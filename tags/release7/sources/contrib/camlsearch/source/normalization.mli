#open "external_type";;
#open "type_for_matching";;

type INTERMEDIARY_TYPE;;

value full_iso : bool ref;;

(* filter type -> normalized_type *)
(* (froz_var_count, subst_var_count, type) -> normalized type *)
value normalize_entry_to_pattern :
  int * int * EXTERNAL_TYPE -> int * int * PATTERN_TYPE;;
value normalize_library_to_pattern :
  int * int * EXTERNAL_TYPE -> int * int * PATTERN_TYPE;;

(* filter type -> normalized_type *)
(* (froz_var_count, subst_var_count, type) -> normalized type *)
value normalize_subject :
  int * int * EXTERNAL_TYPE -> int * SUBJECT_TYPE;;
