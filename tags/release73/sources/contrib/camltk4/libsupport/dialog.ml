#open "tk";;
#open "support";;
#open "protocol";;

let create parent title mesg bitmap def buttons =
  let w = new_widget_atom "toplevel" parent in
  let res = tkEval [|TkToken"tk_dialog";
		     cCAMLtoTKwidget widget_any_table w;
		     TkToken title;
		     TkToken mesg;
	             cCAMLtoTKbitmap bitmap;
	             TkToken (string_of_int def);
	             TkTokenList (map (function x -> TkToken x) buttons)|]
   in
    int_of_string res
;;

let create_named parent name title mesg bitmap def buttons =
  let w = new_named_widget "toplevel" parent name in
  let res = tkEval [|TkToken"tk_dialog";
		     cCAMLtoTKwidget widget_any_table w;
		     TkToken title;
		     TkToken mesg;
	             cCAMLtoTKbitmap bitmap;
	             TkToken (string_of_int def);
	             TkTokenList (map (function x -> TkToken x) buttons)|]
   in
    int_of_string res
;;
