(* builtin to handle callback association to widget *)
let own_set v1 v2 =
tkDo [|TkToken"selection";
       TkToken"own";
       TkTokenList 
      	(map 
      	  (function x -> cCAMLtoTKicccm v2 icccm_selection_ownset_table x)
      	   v1);
       cCAMLtoTKwidget widget_any_table v2|]
;;
