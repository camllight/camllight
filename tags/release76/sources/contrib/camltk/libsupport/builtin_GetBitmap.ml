(* Tk_GetBitmap emulation *)
type Bitmap =
   BitmapFile of string                 (* path of file *)
 | Predefined of string                 (* bitmap  name *)
;;

let CAMLtoTKBitmap = function
  BitmapFile s -> TkToken ("@" ^ s)
| Predefined s -> TkToken s
;;

let TKtoCAMLBitmap s = 
 if nth_char s 0 = `@`
 then BitmapFile (sub_string s 1 (string_length s - 1))
 else Predefined s
;;


