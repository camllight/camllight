(* Color *)
type Color =
     NamedColor of string
   | Black			(* tk keyword: black *)
   | White			(* tk keyword: white *)
   | Red			(* tk keyword: red *)
   | Green			(* tk keyword: green *)
   | Blue			(* tk keyword: blue *)
;;

let CAMLtoTKColor = function
	NamedColor x -> TkToken x
	| Black -> TkToken "black"
	| White -> TkToken "white"
	| Red -> TkToken "red"
	| Green -> TkToken "green"
	| Blue -> TkToken "blue"
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
   XCursor s -> TkToken s
 | XCursorFg (s,fg) -> 
    TkQuote(TkTokenList [TkToken s; CAMLtoTKColor fg])
 | XCursortFgBg (s,fg,bg) ->
    TkQuote(TkTokenList [TkToken s; CAMLtoTKColor fg; CAMLtoTKColor bg])
 | CursorFileFg (s,fg) ->
    TkQuote(TkTokenList [TkToken ("@"^s); CAMLtoTKColor fg])
 | CursorMaskFile (s,m,fg,bg) ->
    TkQuote(TkTokenList [TkToken ("@"^s); TkToken m; CAMLtoTKColor fg; CAMLtoTKColor bg])
;;


