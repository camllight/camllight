(* type *)
type paletteType =
    GrayShades of int
  | RGBShades of int * int * int
;;
(* /type *)

let cCAMLtoTKpaletteType = function
    GrayShades (foo) -> TkToken (string_of_int foo)
  | RGBShades (r,v,b) -> TkToken (string_of_int r^"/"^
				  string_of_int v^"/"^
				  string_of_int b)
;;






















