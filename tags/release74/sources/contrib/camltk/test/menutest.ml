#open "tk";;

let Ciao = function () -> CloseTk () ;exit (0) ;;

let Top = OpenTk () ;;
let a = menubutton__create Top [Text "Menus du pere Dodu"] ;;
let b = menu__create a [] ;;
menu__add_command b [Label "Enfin" ; Command (function () -> print_string "\007";flush std_out)] ;;
menu__add_command b [Label "un menu !!!" ; Command (function () -> print_string "Coucou\n" ; flush std_out)] ;;
menu__add_command b [Label "Bye bye old boy..." ; Command (Ciao)] ;;
menubutton__configure a [Menu b] ;;
pack [a] [] ;;
MainLoop () ;;
