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
  let buf = Send2TkStart false in
  Send2Tk buf "set";
  Send2Tk buf v;
  Send2Tk buf (quote_string x);
  Send2TkEval buf;
  ()
;;

let get v =
  let buf = Send2TkStart true in
  Send2Tk buf result_string_header;
  Send2Tk buf ("set " ^ v);
  Send2Tk buf result_footer;
  let res = Send2TkEval buf in
    res_GetTkString res
;;

let CAMLtoTKTextVariable s = s
;;
