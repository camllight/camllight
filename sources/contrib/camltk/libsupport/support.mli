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

(* The following functions are used internally.
   There is normally no need for them in users programs *)

value dummy_widget : Widget
      	(* used as context *)
;;
          
value widget_name : Widget -> string
      (* Return the name (tk "path") of a widget *)
;;
value new_widget_atom : string -> Widget -> Widget
and   new_named_widget : string -> Widget -> string -> Widget
      (* Abstract creation functions *)
;;

value remove_widget : Widget -> unit
;;

value CAMLtoTKWidget : 'a -> Widget -> string
and   TKtoCAMLWidget : string -> Widget
      (* Conversion functions *)
;;

value Widget_any_table : string list
and Widget_menu_table : string list
and Widget_frame_table : string list
and Widget_entry_table : string list
and chk_sub : string -> 'a list -> 'a -> unit
and check_widget_class : Widget -> string -> unit
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
