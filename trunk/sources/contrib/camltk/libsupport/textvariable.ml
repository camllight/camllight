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
 let res = TkEval [| TkToken "set"; TkToken v |] in
    res_GetTkString res
;;

let CAMLtoTKTextVariable s = TkToken s
;;
