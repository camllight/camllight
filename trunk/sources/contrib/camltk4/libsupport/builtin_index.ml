(* Various indexes
    canvas
    entry
    listbox
*)


(* A large type for all indices in all widgets *)
(* a bit overkill though *)

(* type *)
type index =
	  Number of int		(* no keyword  *)
        | ActiveElement         (* tk keyword: active *)
	| End		        (* tk keyword: end *)
        | Last			(* tk keyword: last *)
        | NoIndex		(* tk keyword: none *)
	| Insert		(* tk keyword: insert *)
	| SelFirst		(* tk keyword: sel.first *)
	| SelLast		(* tk keyword: sel.last *)
        | At of int		(* tk keyword: @n *)
        | AtXY of int * int     (* tk keyword: @x,y *)
        | AnchorPoint   	(* tk keyword: anchor *)
        | Pattern of string     (* no keyword *)
        | LineChar of int * int (* tk keyword: l.c *)
        | Mark of string        (* no keyword *)
        | TagFirst of string    (* tk keyword: tag.first *)
        | TagLast of string     (* tk keyword: tag.last *)
        | Embedded of widget	(* no keyword *)
;;
(* /type *)

(* sp to avoid being picked up by doc scripts *)
 type index_constrs =
	  CNumber
        | CActiveElement
	| CEnd
	| CLast
	| CNoIndex
	| CInsert
	| CSelFirst
	| CSelLast
        | CAt
        | CAtXY
        | CAnchorPoint
	| CPattern
	| CLineChar
	| CMark
	| CTagFirst
	| CTagLast
	| CEmbedded
;;

let index_any_table = 
 [CNumber; CActiveElement; CEnd; CLast; CNoIndex; CInsert; CSelFirst;
  CSelLast; CAt; CAtXY; CAnchorPoint; CPattern; CLineChar;
  CMark; CTagFirst; CTagLast; CEmbedded]
;;

let index_canvas_table =
  [CNumber; CEnd; CInsert; CSelFirst; CSelLast; CAtXY]
;;
let index_entry_table = 
  [CNumber; CAnchorPoint; CEnd; CInsert; CSelFirst; CSelLast; CAt]
;;
let index_listbox_table = 
  [CNumber; CActiveElement; CAnchorPoint; CEnd; CAtXY]
;;
let index_menu_table =
  [CNumber; CActiveElement; CEnd; CLast; CNoIndex; CAt; CPattern]
;;

let index_text_table =
  [CLineChar; CAtXY; CEnd; CMark; CTagFirst; CTagLast; CEmbedded]
;;


let cCAMLtoTKindex table = function
   Number x -> chk_sub "Number" table CNumber; TkToken (string_of_int x)
 | ActiveElement -> chk_sub "ActiveElement" table CActiveElement; TkToken "active"
 | End -> chk_sub "End" table CEnd; TkToken "end"
 | Last -> chk_sub "Last" table CLast; TkToken "last"
 | NoIndex -> chk_sub "NoIndex" table CNoIndex; TkToken "none"
 | Insert -> chk_sub "Insert" table CInsert; TkToken "insert"
 | SelFirst -> chk_sub "SelFirst" table CSelFirst; TkToken "sel.first"
 | SelLast -> chk_sub "SelLast" table CSelLast; TkToken "sel.last"
 | At n -> chk_sub "At" table CAt; TkToken ("@"^string_of_int n)
 | AtXY (x,y) -> chk_sub "AtXY" table CAtXY; 
      	     TkToken ("@"^string_of_int x^","^string_of_int y)
 | AnchorPoint -> chk_sub "AnchorPoint" table CAnchorPoint; TkToken "anchor"
 | Pattern s -> chk_sub "Pattern" table CPattern; TkToken s
 | LineChar (l,c) -> chk_sub "LineChar" table CLineChar;
      	  TkToken (string_of_int l^"."^string_of_int c)
 | Mark s -> chk_sub "Mark" table CMark; TkToken s
 | TagFirst t -> chk_sub "TagFirst" table CTagFirst; 
      	   TkToken (t^".first")
 | TagLast t -> chk_sub "TagLast" table CTagLast;
      	   TkToken (t^".last")
 | Embedded w -> chk_sub "Embedded" table CEmbedded;
       	   cCAMLtoTKwidget widget_any_table w
;;

let char_index c s = 
  let l = string_length s in
  let rec pos i =
    if i >= l then raise Not_found
    else if s.[i] == c then i
    else pos (succ i) in
  pos 0
;;

(* Assume returned values are only numerical and l.c *)
(* .menu index returns none if arg is none, but blast it *)
let cTKtoCAMLindex s =
  try
   let p = char_index `.` s in
    LineChar(int_of_string (sub_string s 0 p), 
      	     int_of_string (sub_string s (p+1) (string_length s - p - 1)))
  with
    Not_found ->
      try Number (int_of_string s)
      with _ -> raise (Invalid_argument ("TKtoCAMLindex: "^s))
;;
