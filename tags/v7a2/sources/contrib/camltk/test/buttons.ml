#open "tk";;


let Ciao = function () -> CloseTk () ;  exit (0) ;;
let Print = function () -> print_string "You've pressed THE button.\n" ; flush std_out ;;

let Top = OpenTk () ;;
let a = label__create Top [Text "-- Buttons test --" ; Relief Raised] ;;

let ChangeLab = function () -> label__configure a [Bitmap (Predefined "error") ; Relief Sunken] ;;

let b = button__create Top [Text "Simple text" ; Relief Groove] ;;
let c = button__create Top [Text "Funny colors" ; ActiveForeground Blue ; ActiveBackground Red] ;;
let d = button__create Top [Bitmap (Predefined "questhead")] ;;
let e = button__create Top [Text "I'm a command" ; Command Print] ;;
let f = button__create Top [Text "Look at the bottom label..." ; Command ChangeLab] ;;
let g = button__create Top [Text "It's boring, bye" ; Command Ciao] ;;
pack [a;b;c;d;e;f;g] [] ;;
MainLoop () ;;
