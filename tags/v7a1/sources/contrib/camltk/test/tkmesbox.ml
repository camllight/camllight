#open "tk";;



let Top = OpenTk () ;;
let f = frame__create Top [] ;;
let r = tk__dialog f ("-") ("Appuyez sur un bouton") ("warning") (1) (["Bouton0" ; "Bouton1" ; "Bouton2"]) in
  print_string ("Bouton "^(string_of_int r)^(" presse.\n")) ; flush std_out ;;
CloseTk () ;
exit (0) ;;
(* For this example, there's no Mainloop !!! *)
