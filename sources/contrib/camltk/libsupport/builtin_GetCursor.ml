(* Color *)
type Color =
	NamedColor of string
	| Black
	| White
	| Red
	| Green
	| Blue
;;

let CAMLtoTKColor = function
	NamedColor x -> quote_string x
	| Black -> "black"
	| White -> "white"
	| Red -> "red"
	| Green -> "green"
	| Blue -> "blue"
;;

let TKtoCAMLColor = function  s -> NamedColor s
;;


(* Tk_GetCursor emulation *)
type Cursor =
   XCursor of string 
 | XCursorFg of string * Color
 | XCursortFgBg of string * Color * Color
 | CursorFileFg of string * Color 
 | CursorMaskFile of string * string * Color * Color
;;

let CAMLtoTKCursor = function
   XCursor s -> quote_string s
 | XCursorFg (s,fg) -> 
    "[list " ^quote_string s^" "^(CAMLtoTKColor fg)^"]"
 | XCursortFgBg (s,fg,bg) ->
    "[list "^quote_string s^" "^(CAMLtoTKColor fg)^" "^(CAMLtoTKColor bg)^"]"
 | CursorFileFg (s,fg) ->
    "[list @\""^s^"\" "^(CAMLtoTKColor fg)^"]"
 | CursorMaskFile (s,m,fg,bg) ->
    "[list @\""^s^"\" \""^m^"\" "^(CAMLtoTKColor fg)^(CAMLtoTKColor bg)^"]"
;;


