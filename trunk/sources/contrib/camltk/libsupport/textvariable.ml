#open "protocol";;

type TextVariable == string
;;

let new =
  let counter = ref 0 in
  function () ->
    incr counter;
    "textv"^ string_of_int !counter
;;

let set v x =
  let _ = TkEval [| TkToken "set"; TkToken v; TkToken x |] in ()
;;
let get v =
  TkEval [| TkToken "set"; TkToken v |]
;;

let CAMLtoTKTextVariable s = TkToken s
;;

let name s = s
;;
