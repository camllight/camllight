(* Compare two strings *)
let string_compare ch1 ch2 =
  match compare_strings ch1 ch2 with
    -1 | -2 -> Inf
  |  1 |  2 -> Sup
  |    _    -> Equ;;

(* Compare two integers *)
let int_compare x y =
  if x < y then
    Inf
  else if x = y then
    Equ
  else
    Sup;;

(* Compare two lists *)
let list_compare compare =
  let rec comparison =
    fun
      [] [] -> Equ
    | (a::l) (b::m) ->
        (match compare a b with
      	   Equ -> comparison l m
	 | x -> x)
    | [] _ -> Inf
    | _ _ -> Sup
  in comparison;;
