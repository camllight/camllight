#open "tk";;            (* Make interface functions available *)
let top = OpenTk ();;   (* Initialisation of the interface *)
(* top is now the toplevel widget *)

(* Widget initialisation *)
let b = button__create top 
          [Text "foobar"; 
           Command (function () -> 
                      print_string "foobar"; 
                      print_newline(); 
                      flush stdout)]
;;
(* b exists but is not yet visible *)
pack [b][] ;;           (* Make b visible *)
MainLoop() ;;           (* User interaction*)
(* You can quit this program by deleting its main window *)
