#open "protocol";;

type TextVariable == string
;;

(* Initialize the variable to avoid error *)
let new =
  let counter = ref 0 in
  function () ->
    incr counter;
    let v = "textv"^ string_of_int !counter in
      TkEval [| TkToken "set"; TkToken v; TkToken "" |];
      v
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
