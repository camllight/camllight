#open "tk";;

let version = "$Id$"
;;

let make_visible atom =
  if winfo__exists atom then
    if not (winfo__ismapped atom) then
      wm__deiconify atom
;;

let valid_component_path str =
  let res = create_string (string_length str) in
  let convertc = function
      `A` .. `Z` as c -> char_of_int (int_of_char c - 32)
    | `a` .. `z` as c -> c
    | `0` .. `9` as c -> c
    | `_` -> `_`
    | _ -> `#` in
  for i = 0 to string_length str - 1 do
    res.[i] <- convertc str.[i]
  done;
  str 
;;




(* Toplevel numbering *)
(* Ceci va poser un probleme avec la table de memorisation des callbacks.
   new_toplevel_widget va toujours *enregistrer* le widget
*)

let new base options =
  (* remove problematic characters *)
  let truebase = valid_component_path base in
  (* loop through numbered windows *)
  let rec find_unused =
    let cnter = ref 1 in
    function () ->
        let w = support__new_toplevel_widget 
      	  (truebase ^ "<" ^ string_of_int !cnter ^ ">") in
	if winfo__exists w then begin
	   incr cnter;
	   find_unused()
	   end
	else w in
  (* create a toplevel widget *)
  let w = support__new_toplevel_widget truebase in
    if winfo__exists w then
      toplevelw__create (find_unused ()) options
    else toplevelw__create w options
;;

(*********** Memo **********)
