(* Auxiliaries for the parser. *)

#open "syntax";;

let regexp_for_string s = re_string 0
  where rec re_string n =
    if n >= string_length s then Epsilon
    else if succ n = string_length s then Characters([nth_char s n])
    else Sequence(Characters([nth_char s n]), re_string (succ n))
;;

let char_class c1 c2 = class (int_of_char c1)
  where rec class n =
    if n > (int_of_char c2) then [] else (char_of_int n) :: class(succ n)
;;

let all_chars = char_class (char_of_int 1) (char_of_int 255)
;;

