(* Run-time support *)

(***************************************************)
(* Widgets *)
(***************************************************)
type widget =
  Untyped of string
| Typed of string * string
;;

(* table of widgets *)
let widget_table = (hashtblc__new 401 : (string,widget) hashtblc__t)
;;

let widget_name = function
    Untyped s -> s
 |  Typed (s,_) -> s
;;

(* Normally all widgets are known *)
(* this is a provision for send commands to external tk processes *)
let widget_class = function
    Untyped _ -> "unknown"
  | Typed (_,c) -> c
;;

(* This one is always created by opentk *)
let default_toplevel_widget =
  let wname = "." in
  let w = Typed (wname, "toplevel") in
    hashtblc__add widget_table wname w;
    w
;;

(* Dummy widget to which global callbacks are associated *)
(* also passed around by CAMLtoTKoption when no widget in context *)
let dummy_widget = 
  Untyped "dummy"
;;

let remove_widget w =
  hashtblc__remove widget_table (widget_name w)
;;

(* Retype widgets returned from Tk *)
(* JPF report: sometime s is "", see Protocol.cTKtoCAMLwidget *)
let get_widget_atom s =
  try
    hashtblc__find widget_table s
  with
    Not_found -> Untyped s
;;

let widget_naming_scheme = [
        "button", "b";
	"canvas", "ca";
	"checkbutton", "cb";
	"entry", "en";
        "frame", "f";
	"label", "l";
	"listbox", "li";
	"menu", "me";
	"menubutton", "mb";
	"message", "ms";
        "radiobutton", "rb";
	"scale", "sc";
	"scrollbar", "sb";
	"text", "t";
      	"toplevel", "top" ]
;;


let widget_any_table =  map fst widget_naming_scheme
;;
(* subtypes *)
let widget_button_table = [ "button" ]
and widget_canvas_table = [ "canvas" ]
and widget_checkbutton_table = [ "checkbutton" ]
and widget_entry_table = [ "entry" ]
and widget_frame_table = [ "frame" ]
and widget_label_table = [ "label" ]
and widget_listbox_table = [ "listbox" ]
and widget_menu_table = [ "menu" ]
and widget_menubutton_table = [ "menubutton" ]
and widget_message_table = [ "message" ]
and widget_radiobutton_table = [ "radiobutton" ]
and widget_scale_table = [ "scale" ]
and widget_scrollbar_table = [ "scrollbar" ]
and widget_text_table = [ "text" ]
and widget_toplevel_table = [ "toplevel" ]
;;

let new_suffix class n =
  try 
    (assoc class widget_naming_scheme) ^ (string_of_int n)
  with
    Not_found -> "w" ^ (string_of_int n)
;;
  

(* The function called by generic creation *)
let new_widget_atom =
  let counter = ref 0 in
  fun class parent ->
      let parentpath = widget_name parent in
      let path = 
      	 incr counter;
	 if parentpath = "."
	 then "." ^ (new_suffix class !counter)
	 else parentpath ^ "." ^ (new_suffix class !counter)
        in
      let w = Typed(path,class) in
	hashtblc__add widget_table path w;
	w
;;


let new_named_widget class parent name =
  let parentpath = widget_name parent in
  let path =
    if parentpath = "."
    then "." ^ name
    else parentpath ^ "." ^ name in
  let w = Typed(path,class) in
	hashtblc__add widget_table path w;
	w
;;
  
(* Just create a path. Only to check existence of widgets *)
(* Use with care *)
let widget_atom parent name =
  let parentpath = widget_name parent in
  let path =
    if parentpath = "."
    then "." ^ name
    else parentpath ^ "." ^ name in
      Untyped path
;;



(* Redundant with subtyping of widget, backward compatibility *)
let check_widget_class = fun
    (Untyped _) _ -> () (* assume run-time check by tk*)
 |  (Typed(_,c)) l ->
       	 if mem c l then ()
      	 else raise (IllegalWidgetType c)
;;


(* Checking membership of constructor in subtype table *)
let chk_sub errname table c =
  if mem c table then ()
  else raise (Invalid_argument errname)
;;

(* strings assumed to be atomic (no space, no special char) *)
let cCAMLtoTKsymbol x = x
;;
let cTKtoCAMLsymbol x = x
;;


(* Extensible buffers *)
type extensible_buffer = {
    mutable buffer : string;
    mutable pos : int;
    mutable len : int}
;;

let new_buffer () = {
   buffer = create_string 128;
   pos = 0;
   len = 128
   }
;;

let print_in_buffer buf s =
  let l = string_length s in
  if buf.pos + l > buf.len then begin
    buf.buffer <- buf.buffer ^ (create_string (l+128));
    buf.len <- buf.len + 128 + l;
    end;
  blit_string s 0 buf.buffer buf.pos l;
  buf.pos <- buf.pos + l
;;

let get_buffer buf = 
  sub_string buf.buffer 0 buf.pos
;;



(* Used by list converters *)
let catenate_sep sep =
  function 
    [] -> ""
  | [x] -> x
  | x::l -> 
      let b = new_buffer() in
      	print_in_buffer b x;
	do_list (function s -> print_in_buffer b sep; print_in_buffer b s) l;
      get_buffer b
;;

(* Parsing results of Tcl *)
(* split a string according to char_sep predicate *)
let split_str char_sep str =
  let len = string_length str in
  if len = 0 then [] else
    let rec skip_sep cur =
      if cur >= len then cur
      else if char_sep str.[cur] then skip_sep (succ cur)
      else cur  in
    let rec split beg cur =
      if cur >= len then 
	if beg = cur then []
	else [sub_string str beg (len - beg)]
      else if char_sep str.[cur] 
	   then 
	     let nextw = skip_sep cur in
	      (sub_string str beg (cur - beg))
		::(split nextw nextw)
	   else split beg (succ cur) in
    let wstart = skip_sep 0 in
    split wstart wstart
;;

