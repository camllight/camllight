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
  TkEval [| TkToken "set"; TkToken v; TkToken x |]; ()
;;
let get v =
  TkEval [| TkToken "set"; TkToken v |]
;;

let CAMLtoTKTextVariable s = TkToken s
;;

let name s = s
;;
