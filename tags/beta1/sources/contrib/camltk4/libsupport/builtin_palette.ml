type PaletteType =
  | GrayShades of int
  | RGBShades of int * int * int
;;

let CAMLtoTKPaletteType = function
    GrayShades (foo) -> TkToken (string_of_int foo)
  | RGBShades (r,v,b) -> TkToken (string_of_int r^"/"^
				  string_of_int v^"/"^
				  string_of_int b)
;;






















