(* Various indexes
    canvas
    entry
    listbox
*)
type Index =
	  Number of int		(* tk keyword:  *)
	| End		        (* tk keyword: end *)
	| Insert		(* tk keyword: insert *)
	| SelFirst		(* tk keyword: sel.first *)
	| SelLast		(* tk keyword: sel.last *)
        | At of int		(* tk keyword: @n *)
        | AtXY of int * int     (* tk keyword: @x,y *)
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
;;

let Index_any_table = 
  [CNumber; CEnd; CInsert; CSelFirst; CSelLast; CAt; CAtXY]
;;
let Index_listbox_table = 
  [CNumber; CEnd; CInsert]
;;
let Index_entry_table = 
  [CNumber; CEnd; CInsert; CSelFirst; CSelLast; CAt]
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
;;

(* In fact, returned values are probably only numerical indexes *)
let TKtoCAMLIndex = function
     "" -> raise (Invalid_argument "TKtoCAMLIndex: empty")
   | "sel.last" -> SelLast
   | "sel.first" -> SelFirst
   | "insert" -> Insert
   | "end" -> End
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
