(* Operations on floating-point numbers *) 

let string_of_float = format_float "%.12g";;

let abs_float f =
  if f <. 0.0 then minus_float f else f
;;
