(* Support for widget manipulations *)
type Widget
      	(* Widget is an abstract type *)
;;

value default_toplevel_widget : Widget
      	(* The default toplevel widget is ".", of type "toplevel" *)
;;

value new_toplevel_widget : string -> Widget
       (* the argument must be a valid window name: no 8bit char, start
          with a lowercase, ... *)
;;

value toplevel_widget_atom : string -> Widget
       (* creates the atom, but does not add the widget in internal tables.
          Allows detection of already existing toplevel widgets. *)
;;

(* The following functions are used internally.
   There is normally no need for them in users programs *)

value dummy_widget : Widget
      	(* used as context *)
;;
          
value widget_name : Widget -> string
      (* Returns the name (tk "path") of a widget *)
and  widget_class : Widget-> string
      (* Returns the class of a widget *)
;;

value new_widget_atom : string -> Widget -> Widget
and   new_named_widget : string -> Widget -> string -> Widget
      (* Abstract creation functions *)
;;

value remove_widget : Widget -> unit
;;

value   TKtoCAMLWidget : string -> Widget
      (* Conversion functions *)
;;

value Widget_any_table : string list
and Widget_button_table : string list
and Widget_canvas_table : string list
and Widget_checkbutton_table : string list
and Widget_entry_table : string list
and Widget_frame_table : string list
and Widget_label_table : string list
and Widget_listbox_table : string list
and Widget_menu_table : string list
and Widget_menubutton_table : string list
and Widget_message_table : string list
and Widget_radiobutton_table : string list
and Widget_scale_table : string list
and Widget_scrollbar_table : string list
and Widget_text_table : string list
and Widget_toplevel_table : string list
and chk_sub : string -> 'a list -> 'a -> unit
and check_widget_class : Widget -> string list -> unit
      (* Widget subtyping *)
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


value quote_string : string -> string
and catenate_sep : string -> string list -> string
and split_str : (char -> bool) -> string -> string list
      (* Various string manipulations *)
;;



type symbol == string
      (* A simpler "string" type *)
;;

value CAMLtoTKsymbol : symbol -> string
and   TKtoCAMLsymbol : string -> symbol
;;
