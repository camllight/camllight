#open "tk";;

let version = "$Id$"
;;

(*
 * Tk 4.0 has emacs bindings for entry widgets
 *)

let new_label_entry parent txt action =
  let f = frame__create parent [] in
  let m = label__create f [Text txt]
  and e = entry__create f [Relief Sunken] in
   tk__bind e [[], XKey "Return"] 
       (BindSet ([], fun _ -> action(entry__get e)));
  pack [m][Side Side_Left];
  pack [e][Side Side_Right; Fill Fill_X; Expand true];
  f,e
;;

