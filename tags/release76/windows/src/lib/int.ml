(* Operations on integers *)

#open "eq";;

let abs n =
  if n < 0 then -n else n
;;

let lnot n =
  n lxor (-1)
;;

let string_of_int = format_int "%ld";;

let min_int = 1 lsl (if 1 lsl 31 = 0 then 30 else 62);;
let max_int = min_int - 1;;

