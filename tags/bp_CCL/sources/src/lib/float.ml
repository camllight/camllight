(* Operations on floating-point numbers *) 

#open "exc";;
#open "fstring";;
#open "fchar";;
#open "int";;

let string_of_float f =
  let s = format_float "%.12g" f in
  try
    for i = 0 to pred(string_length s) do
      match nth_char s i with `.` | `e` | `E` -> raise Exit | _ -> ()
    done;
    s ^ ".0"
  with Exit ->
    s
;;

let abs_float f =
  if f <. 0.0 then minus_float f else f
;;
