#open "type_for_matching";;

exception Wrong_number_of_parameters;;

(* Match `type1' and `type2' *)
(* depth action_on_success type1 type2 -> partial_visit *)
value type_match :
  int -> (unit -> unit) -> (int * int * PATTERN_TYPE) ->
    (int * SUBJECT_TYPE) -> bool;;
