(* TagOrId is builtin  because of SearchSpec *)
type TagOrId =
    Tag of string
  | Id of int
;;


let CAMLtoTKTagOrId = function
   Tag name -> name
 | Id  n -> string_of_int n
;;

let TKtoCAMLTagOrId n = 
  try 
    Id (int_of_string n) 
  with
    _ -> Tag n
;;

type SearchSpec =
    Above of TagOrId			(* tk keyword: above *)
  | All					(* tk keyword: all *)
  | Below of TagOrId			(* tk keyword: below *)
  | Closest of int * int		(* tk keyword: closest *)
  | Enclosed of int * int * int * int	(* tk keyword: enclosed *)
  | Overlapping of int * int * int * int  (* tk keyword: overlapping *)
  | Withtag of TagOrId			(* tk keyword: withtag *)
;;

let CAMLtoTKSearchSpec = function
    Above a -> "above "^(CAMLtoTKTagOrId a)
  | All -> "all "
  | Below a -> "below "^(CAMLtoTKTagOrId a)
  | Closest (a,b) -> "closest "^(string_of_int a)^" "^(string_of_int b)
  | Enclosed (a,b,c,d) -> 
      	"enclosed "^(string_of_int a)^" "^(string_of_int b)^" "
        ^(string_of_int c)^" "^(string_of_int d)
  | Overlapping (a,b,c,d) -> 
      	"overlapping "^(string_of_int a)^" "^(string_of_int b)^" "
        ^(string_of_int c)^" "^(string_of_int d)
  | Withtag a -> "withtag "^(CAMLtoTKTagOrId a)
;;


type CanvasIndex = 
    CI_Number of int		(* tk keyword: *)
  | CI_End			(* tk keyword: end *)
  | CI_Insert			(* tk keyword: insert *)
  | CI_SelFirst			(* tk keyword: sel.first *)
  | CI_SelLast			(* tk keyword: sel.last *)
  | CI_At of int * int		(* tk keyword: @x,y *)
;;


let CAMLtoTKCanvasIndex = function
    CI_Number (bar) -> string_of_int bar
  | CI_End -> "end"
  | CI_Insert -> "insert"
  | CI_SelFirst -> "sel.first"
  | CI_SelLast -> "sel.last"
  | CI_At (foo, bar) -> ("@"^(string_of_int foo)^","^(string_of_int bar))
;;


(* TODO: restrict event fields *)
let canvas_bind widget tag eventsequence action =
  check_widget_class widget "canvas";
  let buf = Send2TkStart false in
  Send2Tk buf (widget_name widget ^ " bind " ^ (CAMLtoTKTagOrId tag) ^ " " ^
      	   (CAMLtoTKEventSequence eventsequence));
  begin match action with
     BindRemove -> Send2Tk buf "{}"
  |  BindSet (what, f) ->
      let CbId = register_callback widget (WrapEventInfo f what) in
        Send2Tk buf (" {camlcb " ^ CbId ^ (WriteEventField what) ^"}")
  |  BindExtend (what, f) ->
      let CbId = register_callback widget (WrapEventInfo f what) in
        Send2Tk buf (" {+camlcb " ^ CbId ^ (WriteEventField what) ^"}")
  end;
  Send2TkEval buf
;;

