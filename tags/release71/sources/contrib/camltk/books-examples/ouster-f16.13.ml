#open "tk";;

(* -textvariable name omitted (no purpose) *)
let top = OpenTk() in
  let label = label__create top [Text "File name:"] in
  let entry = entry__create top 
      	       [TextWidth 20; Relief Sunken; Borderwidth (Pixels 2)] in
    pack [label; entry] 
      	 [Side Side_Left; PadX (Millimeters 1.); PadY (Millimeters 2.)];
    MainLoop()
;;

