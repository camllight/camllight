(* Widget is an abstract type *)
type Widget;;


value default_toplevel_widget : Widget;;
       (* The default toplevel widget is ".", of type "toplevel" *)

value new_toplevel_widget : string -> Widget;;
       (* the argument must be a valid window name: no 8bit char, start
          with a lowercase, ... *)

(* The following functions are used internally.        *)
(* There is normally no need for them in users programs *)
          
value widget_name : Widget -> string;;

value CAMLtoTKWidget : 'a -> Widget -> string;;
value TKtoCAMLWidget : string -> Widget;;

value Widget_any_table : string list;;
value Widget_menu_table : string list;;
value Widget_frame_table : string list;;
value new_widget_atom : string -> Widget -> Widget;;
exception IllegalWidgetType of string
;;
value check_widget_class : Widget -> string -> unit;;

value chk_sub : string -> 'a list -> 'a -> unit;;

value quote_string : string -> string;;
type symbol
;;
value CAMLtoTKsymbol : symbol -> string;;
value TKtoCAMLsymbol : string -> symbol;;

value catenate_sep : string -> string list -> string
;;
value split_str : (char -> bool) -> string -> string list
;;
