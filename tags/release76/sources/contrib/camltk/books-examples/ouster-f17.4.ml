#open "tk";;

let top = OpenTk() in
  let ok = button__create top [Text "OK"] 
  and cancel = button__create top [Text "Cancel"]
  and help = button__create top [Text "Help"] in

    pack [ok; cancel; help] 
      	 [Side Side_Left; PadX (Millimeters 2.); PadY (Millimeters 1.)];

    let act =
      let cnter = ref 0 in
      function _ ->
      	incr cnter;
      	match !cnter with
	  1 -> 
      	  pack [ok; cancel; help] 
      	     [Side Side_Left; IPadX (Millimeters 2.); IPadY (Millimeters 1.)]
	| 2 -> 
      	  pack [ok; cancel; help] 
      	     [Side Side_Left; PadX (Millimeters 2.); PadY (Millimeters 2.);
      	      IPadX (Millimeters 2.); IPadY (Millimeters 2.)]
        | _ -> CloseTk()  in

    bind top [[], KeyPress]
      	 (BindSet ([],act));
    dialog (support__new_toplevel_widget "doit")
     "Explain" "Press a key thrice in the main window" (Predefined "") 0 ["Ok"];
    MainLoop()
;;

