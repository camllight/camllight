(* Tk_GetPixels emulation *)
type Units =
    Pixels of int       (* specified as floating-point, but inconvenient *)
  | Centimeters of float
  | Inches of float
  | Millimeters of float
  | PrinterPoint of float
;;

let CAMLtoTKUnits = function
    Pixels (foo) -> TkToken (string_of_int foo)
  | Millimeters (foo)  -> TkToken(string_of_float foo^"m")
  | Inches (foo)  -> TkToken(string_of_float foo^"i")
  | PrinterPoint (foo) -> TkToken(string_of_float foo^"p")
  | Centimeters (foo) -> TkToken(string_of_float foo^"c")
  ;;

let TKtoCAMLUnits str = 
  let len = string_length str in
  let num_part str = sub_string str 0 (len - 1) in
  match nth_char str (pred len) with
    `c` -> Centimeters (float_of_string (num_part str))
  | `i` -> Inches (float_of_string (num_part str))
  | `m` -> Millimeters (float_of_string (num_part str))
  | `p` -> PrinterPoint (float_of_string (num_part str))
  | _ -> Pixels(int_of_string str)
;;
