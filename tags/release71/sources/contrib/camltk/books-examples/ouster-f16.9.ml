#open "tk";;

let top = OpenTk() in
  let msg = message__create top 
      	       	      [Width (Centimeters 8.);
      	       	       Justify Justify_Left;
		       Relief Raised;
		       Borderwidth (Pixels 2);
		       Font "-Adobe-Helvetica-Medium-R-Normal--*-180-*";
		       Text "You have made changes to this document \
		       since the last time it was saved. Is it OK to \
		       discard the changes?"] in
     pack [msg][];
     MainLoop()
;;
