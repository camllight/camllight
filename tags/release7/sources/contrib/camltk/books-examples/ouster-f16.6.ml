#open "tk";;

(* fake commands omitted *)
let top = OpenTk() in
  let ok = button__create top [Text "OK"] 
  and apply = button__create top [Text "Apply"]   
  and cancel = button__create top [Text "Cancel"]
  and help = button__create top [Text "Help"] in
    pack [ok; apply; cancel; help] [Side Side_Left];
    MainLoop()
;;


