(* type *)
type scrollValue =
	ScrollPage of int		(* tk option: scroll <int> page *)
	| ScrollUnit of int		(* tk option: scroll <int> unit *)
	| MoveTo of float		(* tk option: moveto <float> *)
;;
(* /type *)

let cCAMLtoTKscrollValue = function
   ScrollPage v1 ->
    TkTokenList [TkToken"scroll"; TkToken (string_of_int v1); TkToken"pages"]
 | ScrollUnit v1 ->
    TkTokenList [TkToken"scroll"; TkToken (string_of_int v1); TkToken"units"]
 | MoveTo v1 ->
    TkTokenList [TkToken"moveto"; TkToken (string_of_float v1)]
;;

(* str l -> scrllv -> str l *)
let cTKtoCAMLscrollValue = function
   "scroll"::n::"pages"::l -> 
     ScrollPage (int_of_string n), l
 | "scroll"::n::"units"::l ->
     ScrollUnit (int_of_string n), l
 | "moveto"::f::l ->
     MoveTo (float_of_string f), l
 | _ -> raise (Invalid_argument "TKtoCAMLscrollValue")
;;
