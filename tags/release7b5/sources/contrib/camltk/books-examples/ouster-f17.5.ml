#open "tk";;

let top = OpenTk() in
  let ok = button__create top [Text "OK"] 
  and cancel = button__create top [Text "Cancel Command"]
  and help = button__create top [Text "Help"] in
    pack [ok; cancel; help] [Side Side_Top; Fill Fill_X];
    MainLoop()
;;

