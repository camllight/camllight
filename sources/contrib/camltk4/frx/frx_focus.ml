#open "tk";;

(* Temporary focus *)

(* ? use bind tag ? how about the global reference then *)
let auto w =
  let old_focus = ref w in
  bind w [[],Enter] 
      (BindSet([], fun _ -> old_focus := focus__get (); focus__set w));
  bind w [[],Leave] 
      (BindSet([], fun _ -> focus__set !old_focus))
;;
