(* type *)
type bindings =
	TagBindings of string		(* tk option: <string> *)
	| WidgetBindings of widget		(* tk option: <widget> *)
;;
(* /type *)

let cCAMLtoTKbindings = function
	WidgetBindings v1 -> cCAMLtoTKwidget widget_any_table v1
	| TagBindings v1 -> TkToken v1
;;

(* this doesn't really belong here *)
let cTKtoCAMLbindings s =
  if string_length s > 0 && s.[0] = `.` then
    WidgetBindings (cTKtoCAMLwidget s)
  else TagBindings s
;;
