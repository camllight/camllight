#open "eq";;
#open "int";;
#open "fstring";;

let rec index_char str chr pos =
  if pos >= string_length str then -1
  else if str.[pos] == chr then pos
  else index_char str chr (succ pos)
;;

let string_of_char c = make_string 1 c;;
