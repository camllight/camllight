#open "tk";;


let Bye = function () -> CloseTk () ; exit (0) ;;
let EntryScroll = function a -> function b -> function c -> function d ->
  print_string ((string_of_int a)^" "^(string_of_int b)^" "^(string_of_int c)^" "^(string_of_int d)^"\n") ;
  flush std_out ;;

let Top = OpenTk () ;;
let a = entry__create Top [Relief Sunken ; ScrollCommand EntryScroll] ;;
let b = button__create Top [Text "Salut" ; Background (NamedColor "yellow") ; ActiveForeground Blue ; Command Bye] ;;
pack [a;b] [] ;;
MainLoop () ;;
