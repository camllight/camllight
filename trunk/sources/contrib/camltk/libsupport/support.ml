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
let new_toplevel_widget s = ("toplevel", "." ^ s : Widget)
;;

(* Note: there are subtypes *)
let CAMLtoTKWidget _ = widget_name
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
      	"toplevel", "T" ]
;;
let widget_typing_scheme = 
   map (fun (x,y) -> (y,x)) widget_naming_scheme
;;

let is_digit = function
   `0`..`9` -> true
 | _ -> false
;;

let is_point c = 
   c = `.`
;;

let type_of_widget path =
  let rec lastchar n =
    if is_digit (nth_char path n) then
      lastchar (pred n)
    else n
  and firstchar n =
    if is_point (nth_char path n) then succ n
    else firstchar (pred n) 
  in
  let l = lastchar (string_length path - 1) in
  let f = firstchar l in
   try
     assoc (sub_string path f (l-f+1)) widget_typing_scheme
   with
     Not_found -> raise (Invalid_argument ("invalid widget path :"^path))
;;

(* Retype widgets returned from Tk *)
let TKtoCAMLWidget s =
  (type_of_widget s,s)
;;


let Widget_any_table =  map fst widget_naming_scheme
;;
(* subtypes at this time *)
let Widget_menu_table = [ "menu" ]
;;
let Widget_frame_table = [ "frame" ]
;;


let new_suffix class n =
  try 
    (assoc class widget_naming_scheme) ^ (string_of_int n)
  with
    Not_found -> "w" ^ (string_of_int n)
;;
  

let new_widget_atom =
  let counter = ref 0 in
  fun class parent ->
    if eq_string class "toplevel" then parent
    else begin
      incr counter;
      let parentname = widget_name parent in
	class,
	if parentname = "."
	then "." ^ (new_suffix class !counter)
	else parentname ^ "." ^ (new_suffix class !counter)
      end
;;

(* Redundant with subtyping of Widget *)
(* but used to check types *)
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


