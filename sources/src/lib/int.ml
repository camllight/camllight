(* Operations on integers *)

let min n1 n2 =
  if n1 <= n2 then n1 else n2
;;

let max n1 n2 =
  if n1 >= n2 then n1 else n2
;;

let abs n =
  if n < 0 then -n else n
;;

let lnot n =
  n lxor (-1)
;;

let string_of_int = format_int "%ld";;
