#open "tk";;
#open "listbox";;

let setup () =
  let Top = OpenTk () in
  let Quit = function () -> CloseTk () ; exit 0 in
  let b = button__create Top [Relief Raised ; Text "Exit" ; Command Quit] in
  let s = scrollbar__create Top [ActiveForeground Blue ; Background Green] in
  let l = listbox__create Top [Relief (Raised) ; Background (NamedColor "yellow")] in

  (* APPELEE LORS DE L'UTILISATION DE LA SCROLLBAR POUR METTRE LA LISTBOX A JOUR *)
  let SetLb = function (a:int) -> 
    listbox__yview l (Number(a)) ;
    () in
  (* APPELLE POUR REGLER LA SCROLLBAR EN FONCTION DE LA LISTE *)
  let SetSb a b c d =
     print_string ((string_of_int a)^" "^(string_of_int b)^" "^(string_of_int c)^" "^(string_of_int d)^"\n") ;
     scrollbar__set s a b c d ;
     () in

  listbox__configure l [YScrollCommand (SetSb)] ;
  scrollbar__configure s [Slidecommand (SetLb)] ;

  pack [b] [] ;
  pack [s] [Side Side_Right ; Fill Fill_Y] ;
  pack [l] [] ;
  listbox__insert l End ["Ehhh ??!" ; "What's" ; "up" ; "Doc ?"]
;;

let main () =
  setup() ;
  MainLoop()
;;

printexc__f main () ;;
