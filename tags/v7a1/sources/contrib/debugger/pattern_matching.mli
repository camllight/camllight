(************************ Simple pattern matching ***************)

#open "value";;
#open "globals";;
#open "parser_aux";;

value pattern_matching :
  PATTERN -> VALUE -> typ -> (string * VALUE * typ) list;;
