(* Run-time support *)

(***************************************************)
(* Widgets *)
(***************************************************)
type Widget == string * string
;;

let widget_class (w : Widget) = fst w
;;
let widget_name (w : Widget) = snd w
;;

let default_toplevel_widget =  ("toplevel", "." : Widget)
;;

(* Note: there are subtypes *)
let CAMLtoTKWidget _ = widget_name
;;

(* TODO We should restore the proper type *)
let TKtoCAMLWidget s =
  (s,"unknown")
;;

let Widget_any_table = 
  ["button"; "checkbutton"; "entry"; "listbox"; "canvas"; 
    "menu"; "menubutton"; "radiobutton"; "scale"; "scrollbar"; 
    "text"; "toplevel" ]
;;
let Widget_menu_table = [ "menu" ]
;;
let Widget_frame_table = [ "frame" ]
;;

let widget_naming_scheme =
  ref [ "button", "b";
	"checkbutton", "cb";
	"entry", "en";
	"listbox", "li";
	"canvas", "ca";
	"menu", "m";
	"menubutton", "mb";
        "radiobutton", "rb";
	"scale", "sc";
	"scrollbar", "sb";
	"text", "t";
      	"toplevel", "T" ]
;;

let new_suffix class n =
  try 
    (assoc class !widget_naming_scheme) ^ (string_of_int n)
  with
    Not_found -> "w" ^ (string_of_int n)
;;
  

let new_widget_atom =
  let counter = ref 0 in
  fun class parent ->
    incr counter;
    let parentname = widget_name parent in
      class,
      if parentname = "."
      then "." ^ (new_suffix class !counter)
      else parentname ^ "." ^ (new_suffix class !counter)
;;

(* Redundant with subtyping of Widget *)

let check_widget_class w class =
  if widget_class w = class then ()
  else raise (IllegalWidgetType (widget_class w))
;;


(* Checking membership of constructor in subtype table *)
let chk_sub errname table c =
  if mem c table then ()
  else raise (Invalid_argument errname)
;;

(***************************************************)
(* Callbacks *)
(***************************************************)

let callback_table = 
   (hashtbl__new 73 : (string, unit -> unit) hashtbl__t) 
;;

let new_function_id =
  let counter = ref 0 in
  function () ->
    incr counter;
    "f" ^ (string_of_int !counter)
;;


let register_callback f =
  let id = new_function_id () in
    hashtbl__add callback_table id f;
    id
;;

(* Other builtin types and utilities *)
(* The CAMLtoTKstring converter *)
let quote_string x =
  "\"" ^ x ^ "\""
;;

(* strings assumed to be atomic (no space) *)
type symbol == string
;;

let CAMLtoTKsymbol x = x
;;
let TKtoCAMLsymbol x = x
;;


(* Used by list converters *)
let catenate_sep sep =
  function 
    [] -> ""
  | x::l -> it_list (fun s s' -> s^sep^s') x l
;;


