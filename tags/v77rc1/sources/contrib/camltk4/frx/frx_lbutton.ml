#open "tk";;

let version = "$Id$"
;;

(*
 * Simulate a button with a bitmap AND a label
 *)

let rec sort_options but lab com = function
    [] -> but,lab,com
  |(Command f as o)::l -> sort_options (o::but) lab com l
  |(Bitmap b as o)::l -> sort_options (o::but) lab com l
  |(Text t as o)::l -> sort_options but (o::lab) com l
  |o::l -> sort_options but lab (o::com) l
;;

let create parent options =
  let but,lab,com = sort_options [] [] [] options in
  let f = frame__create parent com in
  let b = button__create f (but@com)
  and l = label__create f (lab@com) in
    pack [b;l][];
    bind l [[],ButtonPressDetail 1] (BindSet ([],(function _ -> button__invoke b)));
    f
;;

let configure f options =
  let but,lab,com = sort_options [] [] [] options in
  match pack__slaves f with
    [b;l] ->
      frame__configure f com;
      button__configure b (but@com);
      label__configure l (lab@com)
  | _ -> raise (Invalid_argument "lbutton configure")
;;
