#open "tk";;

let top = OpenTk () in
 let watch tv =
     let t = toplevelw__create (support__new_toplevel_widget "watch") [] in
     let l = label__create t [Text ("Value of "^textvariable__name tv^" :")]
     and v = label__create t [TextVariable tv] in
       pack [l;v] [Side Side_Left] in


 let country = textvariable__new () in
   textvariable__set country "Japan";
    watch country;
    MainLoop()
;;

(* doesn't work as is, would require a better camltk toplevel *)
(*   textvariable__set country "Great Britain"; *)
