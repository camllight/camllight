type MenuIndex =
    MI_Number of int		
  | MI_Active			(* tk keyword: active *)
  | MI_Last			(* tk keyword: last *)
  | MI_None			(* tk keyword: none *)
  | MI_At of int		(* tk keyword: @x *)
  | MI_Pattern of string
;;


let CAMLtoTKMenuIndex = function
    MI_Active -> TkToken "active"
  | MI_Last -> TkToken "last"
  | MI_None -> TkToken "none"
  | MI_At (foo) -> TkToken ("@"^string_of_int foo)
  | MI_Number (foo) -> TkToken (string_of_int foo)
  | MI_Pattern (foo) -> TkToken foo
;;
