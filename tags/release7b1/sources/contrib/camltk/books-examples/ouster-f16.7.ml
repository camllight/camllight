#open "tk";;

(* -variable does not exist *)
let top = OpenTk() in
  let cbs = map (checkbutton__create top)
      	[[Text "Bold"; Anchor W];
	 [Text "Italic"; Anchor W];
	 [Text "Underline"; Anchor W]] in
  pack cbs [Side Side_Top; Fill Fill_X];
  MainLoop()
;;

