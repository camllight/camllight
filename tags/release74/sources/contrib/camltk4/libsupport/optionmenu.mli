(* Support for tk_optionMenu *)
#open "support";;
#open "textvariable";;

value create: widget -> textVariable -> string list -> widget * widget
      	      (* [create parent var options] creates a multi-option 
      	       	 menubutton and its associated menu. The option is also stored
                 in the variable. Both widgets (menubutton and menu) are
		 returned *)
;;
