(* Not a string as such, more like a symbol *)

type TextMark == string
;;

let CAMLtoTKTextMark  x =  TkToken x
;;
let TKtoCAMLTextMark x = x
;;

type TextTag == string
;;

let CAMLtoTKTextTag  x =  TkToken x
;;
let TKtoCAMLTextTag x = x
;;


type TextModifier =
    CharOffset of int		(* tk keyword: +/- Xchars *)
  | LineOffset of int		(* tk keyword: +/- Xlines *)
  | LineStart			(* tk keyword: linestart *)
  | LineEnd			(* tk keyword: lineend *)
  | WordStart			(* tk keyword: wordstart *)
  | WordEnd 			(* tk keyword: wordend *)
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
   TI_LineChar of int * int		(* tk keyword: l.c *)
 | TI_Mark of TextMark			(* tk keyword: *)
 | TI_End 				(* tk keyword: end *)
 | TI_At of int * int			(* tk keyword: @x,y *)
 | TI_TagFirst of TextTag		(* tk keyword: tag.first *)
 | TI_TagLast of TextTag		(* tk keyword: tag.last *)
;;

let ppBaseTextIndex = function
   TI_LineChar (l,c) -> (string_of_int l) ^ "." ^ (string_of_int c)
 | TI_Mark s -> s
 | TI_End -> "end"
 | TI_At(x,y) -> "@" ^ (string_of_int x) ^ "," ^ (string_of_int y)
 | TI_TagFirst s -> s ^ ".first"
 | TI_TagLast s -> s ^ ".last"
;;

let CAMLtoTKBaseTextIndex i =
  TkToken (ppBaseTextIndex i)
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
    Not_found -> raise (Invalid_argument ("TKtoCAMLBaseTextIndex: "^s))
;;


type TextIndex =
   TextIndex of BaseTextIndex * TextModifier list
 | TextIndexNone
;;

let ppTextIndex = function
   TextIndexNone -> ""
 | TextIndex (base, ml) -> 
     it_list (prefix ^) (ppBaseTextIndex base) (map ppTextModifier ml)
;;

let CAMLtoTKTextIndex i = 
  TkToken (ppTextIndex i)
;;


let text_tag_bind widget tag eventsequence action =
  check_widget_class widget Widget_text_table;
  let _ =
  TkEval [| CAMLtoTKWidget Widget_text_table widget;
            TkToken "tag";
            TkToken "bind";
            CAMLtoTKTextTag tag;
      	    CAMLtoTKEventSequence eventsequence;
  begin match action with
     BindRemove -> TkToken ""
  |  BindSet (what, f) ->
      let CbId = register_callback widget (WrapEventInfo f what) in
        TkToken ("camlcb " ^ CbId ^ (WriteEventField what))
  |  BindExtend (what, f) ->
      let CbId = register_callback widget (WrapEventInfo f what) in
        TkToken ("+camlcb " ^ CbId ^ (WriteEventField what))
  end
  |] in
  ()
;;
