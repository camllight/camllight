type TextModifier =
    CharOffset of int
  | LineOffset of int
  | LineStart
  | LineEnd
  | WordStart
  | WordEnd 
;;

(* TextModifiers are never returned by Tk *)
let ppTextModifier = function
   CharOffset n -> 
      if n > 0 then "+" ^ (string_of_int n) ^ "chars"
      else if n = 0 then ""
      else (string_of_int n) ^ "chars"
 | LineOffset n -> 
      if n > 0 then "+" ^ (string_of_int n) ^ "lines"
      else if n = 0 then ""
      else (string_of_int n) ^ "lines"
 | LineStart -> " linestart"
 | LineEnd -> " lineend"
 | WordStart -> " wordstart"
 | WordEnd -> " wordend"
;;

type BaseTextIndex =
   TI_LineChar of int * int
 | TI_Mark of string
 | TI_End 
 | TI_At of int * int
 | TI_Absolute of int
 | TI_TagFirst of string
 | TI_TagLast of string
;;

let CAMLtoTKBaseTextIndex = function
   TI_LineChar (l,c) -> (string_of_int l) ^ "." ^ (string_of_int c)
 | TI_Mark s -> s
 | TI_End -> "end"
 | TI_At(x,y) -> "@" ^ (string_of_int x) ^ "," ^ (string_of_int y)
 | TI_Absolute n -> string_of_int n
 | TI_TagFirst s -> s ^ ".first"
 | TI_TagLast s -> s ^ ".last"
;;


let char_index c s = find 0
  where rec find i =
    if i >= string_length s 
    then raise Not_found
    else if nth_char s i = c then i 
    else find (i+1) 
;;


(* Assumes only l.c is returned *)
let TKtoCAMLBaseTextIndex s = 
  try
   let p = char_index `.` s in
     TI_LineChar(int_of_string (sub_string s 0 p), 
      	         int_of_string (sub_string s (p+1) (string_length s - p - 1)))
  with 
    Not_found -> raise (Invalid_argument "TKtoCAMLBaseTextIndex")
;;


type TextIndex =
   TextIndex of BaseTextIndex * TextModifier list
 | TextIndexNone
;;

let CAMLtoTKTextIndex = function
   TextIndexNone -> ""
 | TextIndex (base, ml) -> 
     "\"" ^ 
     it_list (prefix ^) (CAMLtoTKBaseTextIndex base) (map ppTextModifier ml)
     ^ "\""
;;


(* Not a string as such, more like a symbol *)
type TextTag == string ;;

let CAMLtoTKTextTag  x =  x
;;
let TKtoCAMLTextTag x = x
;;

type TextMark == string ;;

let CAMLtoTKTextMark  x =  x
;;
let TKtoCAMLTextMark x = x
;;

let text_tag_bind widget tag eventsequence action =
  check_widget_class widget "text";
  Send2TkStart "$PipeTkCallB";
  Send2Tk (widget_name widget ^ " tag bind " ^ (CAMLtoTKTextTag tag) ^ " " ^
      	   (CAMLtoTKEventSequence eventsequence));
  begin match action with
     BindRemove -> Send2Tk " "
  |  BindSet (what, f) ->
      let CbId = register_callback (WrapEventInfo f what) in
      let proc = " {global PipeTkCallB; puts $PipeTkCallB "^CbId^";"
             ^ (WriteEventField what) ^ "flush $PipeTkCallB;}" in
        Send2Tk proc
  end;
  Send2TkEval()
;;
