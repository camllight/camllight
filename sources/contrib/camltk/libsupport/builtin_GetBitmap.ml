(* Tk_GetBitmap emulation *)
type Bitmap =
   BitmapFile of string
 | Predefined of string
;;

let CAMLtoTKBitmap = function
  BitmapFile s -> "\"@" ^ s ^ "\""
| Predefined s -> quote_string s
;;

let TKtoCAMLBitmap s = 
 if nth_char s 0 = `@`
 then BitmapFile (sub_string s 1 (string_length s - 1))
 else Predefined s
;;

