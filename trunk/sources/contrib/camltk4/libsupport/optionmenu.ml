#open "support";;
#open "protocol";;
#open "textvariable";;

(* Implementation of the tk_optionMenu *)

let create parent variable values =
  let w = new_widget_atom "menubutton" parent in
  let mw = new_named_widget "menu" w "menu" in (* assumes .menu naming *)
  let res = 
     tkEval [|TkToken "tk_optionMenu";
              TkToken (widget_name w);
              cCAMLtoTKtextVariable variable;
	      TkTokenList (map (function x -> TkToken x) values)|] in
   if res <> widget_name mw then
     raise (TkError "internal error in optionmenu__create")
   else
     w,mw
;;
