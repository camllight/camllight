#open "tk";;


let Top = OpenTk () ;;
let t = text__create Top [TextWidth 20 ; TextHeight 10 ; Wrap WrapWord] ;;
let Get = function () ->
  print_string (text__get t (TextIndex(TI_LineChar(1,0),[]))
      	       	       	    (TextIndex(TI_End,[])));
  flush std_out ;;
let b = button__create Top [Text "Get" ; Command Get] ;;
let q = button__create Top [Text "Quit" ; Command (function () -> CloseTk () ; exit (0))] ;;

pack [q;b;t] [] ;;
MainLoop () ;;
