type MenuIndex =
    MI_Number of int
  | MI_Active
  | MI_Last
  | MI_None
  | MI_At of int
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
