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

type TextIndex =
   TextIndex of Index * TextModifier list
 | TextIndexNone
;;

let ppTextIndex = function
   TextIndexNone -> ""
 | TextIndex (base, ml) -> 
     let (TkToken ppbase) = CAMLtoTKIndex Index_text_table base in 
       it_list (prefix ^) ppbase (map ppTextModifier ml)
;;

let CAMLtoTKTextIndex i = 
  TkToken (ppTextIndex i)
;;


let text_tag_bind widget tag eventsequence action =
  check_widget_class widget Widget_text_table;
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
  |  BindSetBreakable (what, f) ->
      let CbId = register_callback widget (WrapEventInfo f what) in
        TkToken ("camlcb " ^ CbId ^ (WriteEventField what)^
                 " ; if { $BreakBindingsSequence == 1 } then { break ;} ; set BreakBindingsSequence 0"
                )
  |  BindExtend (what, f) ->
      let CbId = register_callback widget (WrapEventInfo f what) in
        TkToken ("+camlcb " ^ CbId ^ (WriteEventField what))
  end
  |];
  ()
;;


