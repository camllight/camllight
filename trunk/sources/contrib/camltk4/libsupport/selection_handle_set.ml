(* The function *must* use tkreturn *)
let handle_set opts w cmd =
tkDo [|TkToken"selection";
	 TkToken"handle";
	 TkTokenList 
      	   (map 
      	     (function x -> cCAMLtoTKicccm w icccm_selection_handle_table x)
      	     opts);
	cCAMLtoTKwidget widget_any_table w;
      	let id = register_callback w (function args ->
	let (a1,args) = int_of_string (hd args), tl args in
	let (a2,args) = int_of_string (hd args), tl args in
		cmd a1 a2) in TkToken ("camlcb "^id)
|];
()
;;
