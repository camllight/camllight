(* Support for widget manipulations *)
type widget
      	(* widget is an abstract type *)
;;

value default_toplevel_widget : widget
      	(* The default toplevel widget is ".", of type "toplevel" *)
;;

value widget_atom : widget -> string -> widget
;;

(* The following functions are used internally.
   There is normally no need for them in users programs *)

value dummy_widget : widget
      	(* used as context *)
;;
          
value widget_name : widget -> string
      (* Returns the name (tk "path") of a widget *)
and  widget_class : widget-> string
      (* Returns the class of a widget *)
;;

value new_widget_atom : string -> widget -> widget
and   new_named_widget : string -> widget -> string -> widget
      (* Abstract creation functions *)
and get_widget_atom : string -> widget
;;

value remove_widget : widget -> unit
;;


value widget_any_table : string list
and widget_button_table : string list
and widget_canvas_table : string list
and widget_checkbutton_table : string list
and widget_entry_table : string list
and widget_frame_table : string list
and widget_label_table : string list
and widget_listbox_table : string list
and widget_menu_table : string list
and widget_menubutton_table : string list
and widget_message_table : string list
and widget_radiobutton_table : string list
and widget_scale_table : string list
and widget_scrollbar_table : string list
and widget_text_table : string list
and widget_toplevel_table : string list
and chk_sub : string -> 'a list -> 'a -> unit
and check_widget_class : widget -> string list -> unit
      (* widget subtyping *)
;;
exception IllegalWidgetType of string
      (* Raised when widget command applied illegally*)
;;

(* Extensible buffers *)
type extensible_buffer;;
value new_buffer : unit -> extensible_buffer
and   print_in_buffer : extensible_buffer -> string -> unit
and   get_buffer : extensible_buffer -> string
;;


value catenate_sep : string -> string list -> string
and split_str : (char -> bool) -> string -> string list
      (* Various string manipulations *)
;;



type symbol == string
      (* A simpler "string" type *)
;;

value cCAMLtoTKsymbol : symbol -> string
and   cTKtoCAMLsymbol : string -> symbol
;;
