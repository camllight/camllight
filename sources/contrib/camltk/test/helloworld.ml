#open "tk";;
(* Initialisation of the interface *)
let top = OpenTk ();;
(* top is now the toplevel widget *)
(* Widget initialisation *)
let b = button__create top 
          [Text "foobar"; 
	   Command (function () -> print_string "foobar\n"; flush stdout)]
;;
pack [b][]
;;
(* Mainloop for events *)
MainLoop()
;;
