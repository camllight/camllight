(* Character operations, with sanity checks *)

#open "int";;
#open "exc";;

let char_of_int i =
  if i < 0 or i > 255
  then invalid_arg "char_of_int"
  else fchar__char_of_int i
;;

let char_for_read = fchar__char_for_read;;
