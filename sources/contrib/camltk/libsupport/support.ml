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

let default_toplevel_widget =
  let wname = "." in
  let w = Typed (wname, "toplevel") in
    hashtbl__add widget_table wname w;
    w
;;

let new_toplevel_widget s =
  let wname = "."^s in
  let w = Typed(wname, "toplevel") in
    hashtbl__add widget_table wname w;
    w
;;

(* Note: there are subtypes *)
let CAMLtoTKWidget _ = widget_name

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
  


(* Redundant with subtyping of Widget *)
(* but used to check types *)
let check_widget_class = fun
    (Untyped _) _ -> () (* assume run-time check by tk*)
 |  (Typed(_,c)) c' ->
       	 if eq_string c c' then ()
      	 else raise (IllegalWidgetType c)
;;


(* Checking membership of constructor in subtype table *)
let chk_sub errname table c =
  if mem c table then ()
  else raise (Invalid_argument errname)
;;

(* Other builtin types and utilities *)
(* The CAMLtoTKstring converter *)
(* []/$ are still substituted inside "" *) 

let cindex p s start = find start
  where rec find i =
    if i >= string_length s 
    then raise Not_found
    else if p (nth_char s i) then i 
    else find (i+1) 
;;

let must_quote c = 
    c == `[` or c == `]` or c == `$` or c == `{` or c == `}`
;;

let tcl_string_for_read s =
  let s = string_for_read s in
  let rec sfr cur res =
      try
      	let n = cindex must_quote s cur in
	let repl = match nth_char s n with
	            `[` -> "\["
		  | `]` -> "\]"
		  | `$` -> "\$" 
      	       	  | `{` -> "\{"
      	       	  | `}` -> "\}"
      	       	  |  c ->  failwith "subliminal" (* never happens *) in
	let res' = res ^ (sub_string s cur (n-cur)) ^ repl in
	  sfr (succ n) res'
      with Not_found ->
      	res ^ (sub_string s cur (string_length s - cur)) in
  sfr 0 ""
;;

let quote_string x =
  "\"" ^ (tcl_string_for_read x) ^ "\""
;;





(* strings assumed to be atomic (no space, no special char) *)
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

