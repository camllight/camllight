#open "tk";;

let top = OpenTk() in
  let font = textvariable__new () in
  let rbs = map (function otheropts ->
      	       	  radiobutton__create top
		    ([Variable font; Anchor W] @ otheropts))
	        [[Text "Times"; Value "times"];
		 [Text "Helvetica"; Value "helvetica"];
		 [Text "Courier"; Value "courier"];
		 [Text "Symbol"; Value "symbol"]] in
    pack rbs [Side Side_Top; Fill Fill_X];
    MainLoop()
;;

		 
