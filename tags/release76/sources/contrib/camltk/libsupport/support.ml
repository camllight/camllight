(* Run-time support *)

(***************************************************)
(* Widgets *)
(***************************************************)
type Widget =
  Untyped of string
| Typed of string * string
;;

(* table of widgets *)
let widget_table = (hashtbl__new 37 : (string,Widget) hashtbl__t)
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
    hashtbl__add widget_table wname w;
    w
;;

(* Dummy widget to which global callbacks are associated *)
(* also passed around by CAMLtoTKoption when no widget in context *)
let dummy_widget = 
  default_toplevel_widget
;;

let toplevel_widget_atom s = 
  Typed("."^s, "toplevel")
;;

let new_toplevel_widget s =
  let wname = "."^s in
  let w = Typed(wname, "toplevel") in
    hashtbl__add widget_table wname w;
    w
;;

let remove_widget w =
  hashtbl__remove widget_table (widget_name w)
;;



(* Retype widgets returned from Tk *)
let TKtoCAMLWidget s =
  try
    hashtbl__find widget_table s
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
      	"toplevel", "T" ]
;;


let Widget_any_table =  map fst widget_naming_scheme
;;
(* subtypes *)
let Widget_button_table = [ "button" ]
and Widget_canvas_table = [ "canvas" ]
and Widget_checkbutton_table = [ "checkbutton" ]
and Widget_entry_table = [ "entry" ]
and Widget_frame_table = [ "frame" ]
and Widget_label_table = [ "label" ]
and Widget_listbox_table = [ "listbox" ]
and Widget_menu_table = [ "menu" ]
and Widget_menubutton_table = [ "menubutton" ]
and Widget_message_table = [ "message" ]
and Widget_radiobutton_table = [ "radiobutton" ]
and Widget_scale_table = [ "scale" ]
and Widget_scrollbar_table = [ "scrollbar" ]
and Widget_text_table = [ "text" ]
and Widget_toplevel_table = [ "toplevel" ]
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
  fun 
    "toplevel" w -> w  (* toplevel widgets are given in argument *)
  | class parent ->
      let parentpath = widget_name parent in
      let path = 
      	 incr counter;
	 if parentpath = "."
	 then "." ^ (new_suffix class !counter)
	 else parentpath ^ "." ^ (new_suffix class !counter)
        in
      let w = Typed(path,class) in
	hashtbl__add widget_table path w;
	w
;;

let new_named_widget class parent name =
  let parentpath = widget_name parent in
  let path =
    if parentpath = "."
    then "." ^ name
    else parentpath ^ "." ^ name in
  let w = Typed(path,class) in
	hashtbl__add widget_table path w;
	w
;;
  


(* Redundant with subtyping of Widget, backward compatibility *)
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

(* 
 * Other builtin types and utilities
 *   The CAMLtoTKstring converter : []/$ are still substituted inside "" 
 *   Dead code since we now bypass Tcl_Eval
 *) 

let tcl_string_for_read s =
  let n = ref 0 in
    for i = 0 to string_length s - 1 do
      n := !n +
        (match nth_char s i with
           `"` | `\\` | `\n` | `\t` | `[` | `]` | `$` | `{` | `}` -> 2
          | c -> if is_printable c then 1 else 4)
    done;
    if !n == string_length s then s else begin
      let s' = create_string !n in
        n := 0;
        for i = 0 to string_length s - 1 do
          begin
            match nth_char s i with
              `"` -> s'.[!n] <- `\\`; incr n; s'.[!n] <- `"`
            | `\\` -> s'.[!n] <- `\\`; incr n; s'.[!n] <- `\\`
            | `\n` -> s'.[!n] <- `\\`; incr n; s'.[!n] <- `n`
            | `\t` -> s'.[!n] <- `\\`; incr n; s'.[!n] <- `t`
            | `[` -> s'.[!n] <- `\\`; incr n; s'.[!n] <- `[`
            | `]` -> s'.[!n] <- `\\`; incr n; s'.[!n] <- `]`
            | `$` -> s'.[!n] <- `\\`; incr n; s'.[!n] <- `$`
            | `{` -> s'.[!n] <- `\\`; incr n; s'.[!n] <- `{`
            | `}` -> s'.[!n] <- `\\`; incr n; s'.[!n] <- `}`
            | c ->
                if is_printable c then
                  s'.[!n] <- c
                else begin
                  let a = int_of_char c in
                  s'.[!n] <- `\\`;
                  incr n;
                  s'.[!n] <- (char_of_int (48 + a / 100));
                  incr n;
                  s'.[!n] <- (char_of_int (48 + (a / 10) mod 10));
                  incr n;
                  s'.[!n] <- (char_of_int (48 + a mod 10))
                end
          end;
          incr n
        done;
        s'
      end
;;

let quote_string x =
  let n = string_length x + 2 in
  let s = create_string n in
    s.[0] <- ` `;
    s.[n-1] <- ` `;
    blit_string x 0 s 1 (n-2);
  let s' = tcl_string_for_read s in
    s'.[0] <- `"`;
    s'.[string_length s'-1] <- `"`;
   s'
;;


(* strings assumed to be atomic (no space, no special char) *)
let CAMLtoTKsymbol x = x
;;
let TKtoCAMLsymbol x = x
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
  let rec skip_sep cur =
    if cur >= len then cur
    else if char_sep (nth_char str cur) then skip_sep (succ cur)
    else cur  in
  let rec split beg cur =
    if cur >= len then 
      if beg = cur then []
      else [sub_string str beg (len - beg)]
    else if char_sep (nth_char str cur) 
         then 
      	   let nextw = skip_sep cur in
      	    (sub_string str beg (cur - beg))
      	      ::(split nextw nextw)
	 else split beg (succ cur) in
  let wstart = skip_sep 0 in
  split wstart wstart
;;

