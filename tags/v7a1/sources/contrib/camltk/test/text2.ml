#open "tk";;

let Top = OpenTk () ;;
let TextW = text__create Top [TextWidth 20] ;;
let ScrollW = scrollbar__create Top [] ;;
let ByeW = button__create Top [Text "Quit" ; Command (function () -> CloseTk () ; exit 0)] ;;

let TextScrollCB = scrollbar__set ScrollW
;;

let ScrollBCB = text__yview_line TextW
;;

scrollbar__configure ScrollW [Slidecommand ScrollBCB] ;;
text__configure TextW [YScrollCommand TextScrollCB] ;;

pack [ScrollW] [Fill Fill_Y;Side Side_Right] ;;
pack [TextW] [] ;;
pack [ByeW] [] ;;

MainLoop () ;;
