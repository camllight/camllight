type MenuIndex =
    MI_Number of int		
  | MI_Active			(* tk keyword: active *)
  | MI_Last			(* tk keyword: last *)
  | MI_None			(* tk keyword: none *)
  | MI_At of int		(* tk keyword: @x *)
  | MI_Pattern of string
;;


let CAMLtoTKMenuIndex = function
    MI_Active -> "active"
  | MI_Last -> "last"
  | MI_None -> "none"
  | MI_At (foo) -> "@"^(string_of_int foo)
  | MI_Number (foo) -> string_of_int foo
  | MI_Pattern (foo) -> foo
;;
