type Index =
	  Number of int		(* tk keyword:  *)
	| End		        (* tk keyword: end *)
	| Insert		(* tk keyword: insert *)
	| SelFirst		(* tk keyword: sel.first *)
	| SelLast		(* tk keyword: sel.last *)
        | At of int		(* tk keyword: @n *)
;;

type Index_constrs =
	  CNumber
	| CEnd
	| CInsert
	| CSelFirst
	| CSelLast
        | CAt
;;

let Index_any_table = [CNumber; CEnd; CInsert; CSelFirst; CSelLast; CAt]
;;
let Index_listbox_table = [CNumber; CEnd; CInsert]
;;
let Index_entry_table = [CNumber; CEnd; CInsert; CSelFirst; CSelLast; CAt]
;;
let CAMLtoTKIndex table = function
	  Number x -> chk_sub "Number" table CNumber; string_of_int x
	| End -> chk_sub "End" table CEnd; "end"
	| Insert -> chk_sub "Insert" table CInsert; "insert"
	| SelFirst -> chk_sub "SelFirst" table CSelFirst; "sel.first"
	| SelLast -> chk_sub "SelLast" table CSelLast; "sel.last"
	| At n -> chk_sub "At" table CAt; "@"^(string_of_int n)
;;

let TKtoCAMLIndex = function
     "" -> raise (Invalid_argument "TKtoCAMLIndex")
   | "sel.last" -> SelLast
   | "sel.first" -> SelFirst
   | "insert" -> Insert
   | "end" -> End
   | s when s.[0] = `@` -> 
       begin try
         At(int_of_string (sub_string s 1 (pred (string_length s))))
       with
	 _ -> raise (Invalid_argument "TKtoCAMLIndex")
       end
   | s -> 
       begin try Number(int_of_string s)
       with _ -> raise (Invalid_argument "TKtoCAMLIndex")
       end
;;
