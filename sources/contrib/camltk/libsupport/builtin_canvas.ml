type TagOrId =
    Tag of string
  | Id of int ;;


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

let GetTagOrId port =
  TKtoCAMLTagOrId (GetTkToken port)
;;

let GetTagOrIdList port =
  map TKtoCAMLTagOrId (GetTkTokenList port)
;;



type SearchSpec =
    Above of TagOrId
  | All
  | Below of TagOrId
  | Closest of int * int
  | Enclosed of int * int * int * int
  | Overlapping of int * int * int * int
  | Withtag of TagOrId
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
    CI_Number of int
  | CI_End
  | CI_Insert
  | CI_SelFisrt
  | CI_SelLast
  | CI_At of int * int
;;


let CAMLtoTKCanvasIndex = function
    CI_Number (bar) -> string_of_int bar
  | CI_End -> "end"
  | CI_Insert -> "insert"
  | CI_SelFisrt -> "sel.first"
  | CI_SelLast -> "sel.last"
  | CI_At (foo, bar) -> ("@"^(string_of_int foo)^","^(string_of_int bar))
;;


let canvas_bind widget tag eventsequence action =
  check_widget_class widget "canvas";
  Send2TkStart "$PipeTkCallB";
  Send2Tk (widget_name widget ^ " bind " ^ (CAMLtoTKTagOrId tag) ^ " " ^
      	   (CAMLtoTKEventSequence eventsequence));
  begin match action with
     BindRemove -> Send2Tk " "
  |  BindSet (what, f) ->
      let CbId = register_callback (WrapEventInfo f what) in
      let proc = " {global PipeTkCallB; puts $PipeTkCallB "^CbId^";"
             ^ (WriteEventField what) ^ "flush $PipeTkCallB;}" in
        Send2Tk proc
  end;
  Send2TkEval()
;;


type CanvasCoordinates =
    Pixels of float
  | Millimeters of float
  | Inches of float
  | PrinterPoint of float
  | Centimeters of float
;;


let CAMLtoTKCanvasCoordinates = function
    Pixels (foo) -> (string_of_float foo)
  | Millimeters (foo)  -> (string_of_float foo)^"m"
  | Inches (foo)  -> (string_of_float foo)^"i"
  | PrinterPoint (foo) -> (string_of_float foo)^"p"
  | Centimeters (foo) -> (string_of_float foo)^"c"
;;

(* Tk always returns pixels *)
let TKtoCAMLCanvasCoordinates q = 
  Pixels (float_of_string q)
;;

let GetCanvasCoordinates port = 
  TKtoCAMLCanvasCoordinates (GetTkToken port)
;;

let GetCanvasCoordinatesList port = 
 map TKtoCAMLCanvasCoordinates (GetTkTokenList port)
;;
