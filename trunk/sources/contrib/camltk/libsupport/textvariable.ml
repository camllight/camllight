#open "protocol";;
#open "support";;


type TextVariable == string
;;

let new =
  let counter = ref 0 in
  function () ->
    incr counter;
    "textv"^ string_of_int !counter
;;

let set v x =
  Send2TkStart "$PipeTkCallB";
  Send2Tk "set";
  Send2Tk v;
  Send2Tk (quote_string x);
  Send2TkEval();
  ()
;;

let get v =
  Send2TkStart "$PipeTkResult";
  Send2Tk "nputs $PipeTkResult";
  Send2Tk ("$" ^ v);
  Send2Tk "; flush $PipeTkResult";
  Send2TkEval();
  GetTkString !PipeTkResult;;

let CAMLtoTKTextVariable s = s
;;
