type PaletteType =
  | NumberOfGrayShades of int
  | NumberOfRGBShades of int * int * int
;;

let CAMLtoTKPaletteType = function
    NumberOfGrayShades (foo) -> TkToken (string_of_int foo)
  | NumberOfRGBShades (r,v,b) -> TkToken ((string_of_int r)^"/"^
                                         (string_of_int v)^"/"^
                                         (string_of_int b))
;;






















