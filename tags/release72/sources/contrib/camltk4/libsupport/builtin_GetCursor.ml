(* Color *)
(* type *)
type color =
     NamedColor of string
   | Black			(* tk keyword: black *)
   | White			(* tk keyword: white *)
   | Red			(* tk keyword: red *)
   | Green			(* tk keyword: green *)
   | Blue			(* tk keyword: blue *)
   | Yellow                     (* tk keyword: yellow *)
;;
(* /type *)

let cCAMLtoTKcolor = function
	NamedColor x -> TkToken x
	| Black -> TkToken "black"
	| White -> TkToken "white"
	| Red -> TkToken "red"
	| Green -> TkToken "green"
	| Blue -> TkToken "blue"
        | Yellow -> TkToken "yellow"
;;

let cTKtoCAMLcolor = function  s -> NamedColor s
;;


(* Tk_GetCursor emulation *)
(* type *)
type cursor =
   XCursor of string 
 | XCursorFg of string * color
 | XCursortFgBg of string * color * color
 | CursorFileFg of string * color 
 | CursorMaskFile of string * string * color * color
;;
(* /type *)

let cCAMLtoTKcursor = function
   XCursor s -> TkToken s
 | XCursorFg (s,fg) -> 
    TkQuote(TkTokenList [TkToken s; cCAMLtoTKcolor fg])
 | XCursortFgBg (s,fg,bg) ->
    TkQuote(TkTokenList [TkToken s; cCAMLtoTKcolor fg; cCAMLtoTKcolor bg])
 | CursorFileFg (s,fg) ->
    TkQuote(TkTokenList [TkToken ("@"^s); cCAMLtoTKcolor fg])
 | CursorMaskFile (s,m,fg,bg) ->
    TkQuote(TkTokenList [TkToken ("@"^s); TkToken m; cCAMLtoTKcolor fg; cCAMLtoTKcolor bg])
;;


