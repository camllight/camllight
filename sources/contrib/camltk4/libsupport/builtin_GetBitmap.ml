(* Tk_GetBitmap emulation *)
(* type *)
type bitmap =
   BitmapFile of string                 (* path of file *)
 | Predefined of string                 (* bitmap  name *)
;;
(* /type *)

let cCAMLtoTKbitmap = function
  BitmapFile s -> TkToken ("@" ^ s)
| Predefined s -> TkToken s
;;

let cTKtoCAMLbitmap s = 
 if s = "" then Predefined ""
 else if s.[0] = `@`
 then BitmapFile (sub_string s 1 (string_length s - 1))
 else Predefined s
;;


