(* Not a string as such, more like a symbol *)

(* type *)
type textMark == string
;;
(* /type *)

let cCAMLtoTKtextMark  x =  TkToken x
;;
let cTKtoCAMLtextMark x = x
;;

(* type *)
type textTag == string
;;
(* /type *)

let cCAMLtoTKtextTag  x =  TkToken x
;;
let cTKtoCAMLtextTag x = x
;;

(* type *)
type textModifier =
    CharOffset of int		(* tk keyword: +/- Xchars *)
  | LineOffset of int		(* tk keyword: +/- Xlines *)
  | LineStart			(* tk keyword: linestart *)
  | LineEnd			(* tk keyword: lineend *)
  | WordStart			(* tk keyword: wordstart *)
  | WordEnd 			(* tk keyword: wordend *)
;;
(* /type *)

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

(* type *)
type textIndex =
   TextIndex of index * textModifier list
 | TextIndexNone
;;
(* /type *)

let ppTextIndex = function
   TextIndexNone -> ""
 | TextIndex (base, ml) -> 
     match cCAMLtoTKindex index_text_table base with
     | TkToken ppbase -> it_list (prefix ^) ppbase (map ppTextModifier ml)
     | _ -> raise (TkError "ppTextIndex")
;;

let cCAMLtoTKtextIndex i = 
  TkToken (ppTextIndex i)
;;

