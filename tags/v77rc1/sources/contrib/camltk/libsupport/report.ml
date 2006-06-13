(* Report globals from protocol to tk *)
let OpenTk = OpenTk
and OpenTkClass = OpenTkClass
and OpenTkDisplayClass = OpenTkDisplayClass
and CloseTk = CloseTk
and MainLoop = MainLoop
;;

let add_fileinput = add_fileinput
and remove_fileinput = remove_fileinput
;;

let CAMLtoTKWidget table w = 
  check_widget_class w table;
  TkToken (widget_name w)
;;
