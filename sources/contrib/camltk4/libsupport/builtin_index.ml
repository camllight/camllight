(* Various indexes
    canvas
    entry
    listbox
*)

(* This type is here because of dependencies with Anchorage *)
type Anchor =
	SE		(* tk option: se *)
	| S		(* tk option: s *)
	| SW		(* tk option: sw *)
	| E		(* tk option: e *)
	| Center		(* tk option: center *)
	| W		(* tk option: w *)
	| NE		(* tk option: ne *)
	| N		(* tk option: n *)
	| NW		(* tk option: nw *)
;;

let CAMLtoTKAnchor = function
	SE -> TkToken"se"
	| S -> TkToken"s"
	| SW -> TkToken"sw"
	| E -> TkToken"e"
	| Center -> TkToken"center"
	| W -> TkToken"w"
	| NE -> TkToken"ne"
	| N -> TkToken"n"
	| NW -> TkToken"nw"
;;



type Index =
	  Number of int		(* tk keyword:  *)
	| End		        (* tk keyword: end *)
	| Insert		(* tk keyword: insert *)
	| SelFirst		(* tk keyword: sel.first *)
	| SelLast		(* tk keyword: sel.last *)
        | At of int		(* tk keyword: @n *)
        | AtXY of int * int     (* tk keyword: @x,y *)
(* New for TK 4.0 *)
        | Active                (* tk keyword: active *)
        | Anchorage of Anchor   (* Problem of dependance *)
;;

(* sp to avoid being picked up by doc scripts *)
 type Index_constrs =
	  CNumber
	| CEnd
	| CInsert
	| CSelFirst
	| CSelLast
        | CAt
        | CAtXY
(* New for TK 4.0 *)
        | CActive
        | CAnchorage
;;

let Index_any_table = 
  [CNumber; CEnd; CInsert; CSelFirst; CSelLast; CAt; CAtXY]
;;
let Index_listbox_table = 
  [CNumber; CEnd; CInsert; CActive]
;;
let Index_entry_table = 
  [CNumber; CEnd; CInsert; CSelFirst; CSelLast; CAt; CAnchorage]
;;
let Index_canvas_table =
  [CNumber; CEnd; CInsert; CSelFirst; CSelLast; CAtXY]
;;
 
let CAMLtoTKIndex table = function
   Number x -> chk_sub "Number" table CNumber; TkToken (string_of_int x)
 | End -> chk_sub "End" table CEnd; TkToken "end"
 | Insert -> chk_sub "Insert" table CInsert; TkToken "insert"
 | SelFirst -> chk_sub "SelFirst" table CSelFirst; TkToken "sel.first"
 | SelLast -> chk_sub "SelLast" table CSelLast; TkToken "sel.last"
 | At n -> chk_sub "At" table CAt; TkToken ("@"^string_of_int n)
 | AtXY (x,y) -> chk_sub "AtXY" table CAtXY; 
      	     TkToken ("@"^string_of_int x^","^string_of_int y)
 | Active -> chk_sub "Active" table CActive;
       	TkToken "active"
 | Anchorage (a) -> chk_sub "Anchorage" table CAnchorage;
        CAMLtoTKAnchor a
;;

(* In fact, returned values are probably only numerical indexes *)
let TKtoCAMLIndex = function
     "" -> raise (Invalid_argument "TKtoCAMLIndex: empty")
   | "sel.last" -> SelLast
   | "sel.first" -> SelFirst
   | "insert" -> Insert
   | "end" -> End
   | "active" -> Active
   (* Anchor part *)
   | "se" -> Anchorage SE
   | "s" -> Anchorage S
   | "sw" -> Anchorage SW
   | "e" -> Anchorage E
   | "center" -> Anchorage Center
   | "w" -> Anchorage W
   | "ne" -> Anchorage NE
   | "n" -> Anchorage N
   | "nw" -> Anchorage NW
   (* Anchorage end *)
   | s when s.[0] = `@` -> 
       begin try
         At(int_of_string (sub_string s 1 (pred (string_length s))))
       with
	 _ -> raise (Invalid_argument ("TKtoCAMLIndex: "^s))
       end
   | s -> 
       begin try Number(int_of_string s)
       with _ -> raise (Invalid_argument ("TKtoCAMLIndex: "^s))
       end
;;
