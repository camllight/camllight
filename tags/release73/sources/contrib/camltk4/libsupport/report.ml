(* Report globals from protocol to tk *)
let openTk = openTk
and openTkClass = openTkClass
and openTkDisplayClass = openTkDisplayClass
and closeTk = closeTk
and mainLoop = mainLoop
;;

let add_fileinput = add_fileinput
and remove_fileinput = remove_fileinput
and add_fileoutput = add_fileoutput
and remove_fileoutput = remove_fileoutput
;;

let add_timer = add_timer
and remove_timer = remove_timer
;;

let cCAMLtoTKwidget table w = 
  check_widget_class w table;
  TkToken (widget_name w)
;;
