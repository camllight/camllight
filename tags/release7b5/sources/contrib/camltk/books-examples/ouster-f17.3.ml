#open "tk";;

let top = OpenTk() in
  let ok = button__create top [Text "OK"] 
  and cancel = button__create top [Text "Cancel"]
  and help = button__create top [Text "Help"] in
    pack [ok; cancel; help] [Side Side_Left];

    let act =
      let cnter = ref 0 in
      function _ ->
      	incr cnter;
      	match !cnter with
	  1 -> button__configure cancel [Text "Cancel Command"]
	| 2 -> pack [ok; cancel; help] [Side Side_Top]
        | _ -> CloseTk()  in

    bind top [[], KeyPress]
      	 (BindSet ([],act));
    dialog (support__new_toplevel_widget "doit")
      	   "Explain" "Press a key thrice in the main window" (Predefined "") 0 ["Ok"];
    MainLoop()
;;
