#open "tk";;

(* A simple dialog example *)

let main () =
  let top = OpenTk() in
  let r = dialog (support__new_toplevel_widget "dialog")
      	       	 "The title"
      	       	 "Dialog example"	(* Text in the dialog window *)
		 (Predefined "warning") (* Bitmap *)
      	       	 1			(* Default button *)
		 ["Button 0"; "Button 1"; "Button 2"] in
  (* This is modal interaction, so we don't need to call MainLoop() *)
  print_string "You pressed button "; print_int r; print_newline();
  flush std_out;
  CloseTk()
;;

printexc__f main ()
;;
