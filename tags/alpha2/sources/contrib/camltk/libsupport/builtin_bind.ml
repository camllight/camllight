(* Events and bindings *)

(* Builtin types *)
type XEvent =
    XKey of string	(* /usr/include/X11/keysymdef.h *)
  | ButtonPress
  | Button
  | ButtonRelease
  | Circulate
  | CirculateRequest
  | ColorMap
  | Configure
  | ConfigureRequest
  | Destroy
  | Enter
  | Expose
  | FocusIn
  | FocusOut
  | Gravity
  | Keymap
  | KeyPress
  | Key
  | KeyRelease
  | MapRequest
  | Motion
  | Leave
  | Map
  | Property
  | Reparent
  | ResizeRequest
  | Unmap
  | Visibility 
  | WhatButton of int
;;

let CAMLtoTKXEvent = function
    XKey a -> a
  | ButtonPress -> "ButtonPress"
  | Button -> "Button"
  | ButtonRelease -> "ButtonRelease"
  | Circulate -> "Circulate"
  | CirculateRequest -> "CirculateRequest"
  | ColorMap -> "ColorMap"
  | Configure -> "Configure"
  | ConfigureRequest -> "ConfigureRequest"
  | Destroy -> "Destroy"
  | Enter -> "Enter"
  | Expose -> "Expose"
  | FocusIn -> "FocusIn"
  | FocusOut -> "FocusOut"
  | Gravity -> "Gravity"
  | Keymap -> "Keymap"
  | KeyPress -> "KeyPress"
  | Key -> "Key"
  | KeyRelease -> "KeyRelease"
  | MapRequest -> "MapRequest"
  | Motion -> "Motion"
  | Leave -> "Leave"
  | Map -> "Map"
  | Property -> "Property"
  | Reparent -> "Reparent"
  | ResizeRequest -> "ResizeRequest"
  | Unmap -> "Unmap"
  | Visibility -> "Visibility" 
  | WhatButton n -> string_of_int n

;;

type Modifier =
    Control
  | Shift
  | Lock
  | Button1
  | Button2
  | Button3
  | Button4
  | Button5
  | Any
  | Double
  | Triple
  | Mod1
  | Mod2
  | Mod3
  | Mod4
  | Mod5
  | Meta
  | Alt ;;


let CAMLtoTKModifier = function
   Control ->"Control-"
 | Shift -> "Shift-"
 | Lock -> "Lock-"
 | Button1 -> "Button1-"
 | Button2 -> "Button2-"
 | Button3 -> "Button3-"
 | Button4 -> "Button4-"
 | Button5 -> "Button5-"
 | Any -> "Any-"
 | Double -> "Double-"
 | Triple -> "Triple-"
 | Mod1 -> "Mod1-"
 | Mod2 -> "Mod2-"
 | Mod3 -> "Mod3-"
 | Mod4 -> "Mod4-"
 | Mod5 -> "Mod5-"
 | Meta -> "Meta-"
 | Alt -> "Alt-"
;;


(* type Event = Modifier list * XEvent *)
let CAMLtoTKEvent (ml, xe) =
  "<" ^ (catenate_sep " " (map CAMLtoTKModifier ml))  
      ^ (CAMLtoTKXEvent xe) ^ ">"
;;
  
(* type EventSequence == (Modifier list * XEvent) list *)
let CAMLtoTKEventSequence l =
  it_list (prefix ^) "" (map CAMLtoTKEvent l)
;;

(* Event structure, passed to bounded functions *)

type EventInfo = {
  mutable Ev_Above : int;
  mutable Ev_ButtonNumber : int;
  mutable Ev_Count : int;
  mutable Ev_Detail : string;
  mutable Ev_Focus : bool;
  mutable Ev_Height : int;
  mutable Ev_KeyCode : int;
  mutable Ev_Mode : string;
  mutable Ev_State : string;
  mutable Ev_ValueMask : int;
  mutable Ev_Width : int;
  mutable Ev_MouseX : int;
  mutable Ev_MouseY : int;
  mutable Ev_WidgetName : string;
  mutable Ev_Time : string;
  mutable Ev_OverrideRedirect : string;
  mutable Ev_Place : string;
  mutable Ev_Char : string;
  mutable Ev_BorderWidth : int;
  mutable Ev_Display : int;
  mutable Ev_SendEvent : int;
  mutable Ev_KeySymString : string;
  mutable Ev_KeySymInt : int;
  mutable Ev_RootWindow : int;
  mutable Ev_SubWindow : int;
  mutable Ev_Type : int;
  mutable Ev_RootX : int;
  mutable Ev_RootY : int };;


type EventField =
    Above
  | ButtonNumber
  | Count
  | Detail
  | Focus
  | Height
  | KeyCode
  | Mode
  | State
  | ValueMask
  | Width
  | MouseX
  | MouseY
  | WidgetName
  | Time 
  | OverrideRedirect
  | Place
  | Char
  | BorderWidth
  | Display
  | SendEvent
  | KeySymString
  | KeySymInt
  | RootWindow
  | SubWindow
  | Type
  | RootX
  | RootY;;

(* TODO: check types ! *)

let FillEventInfo ev = function 
    Above 	-> ev.Ev_Above 		<- int_of_string (GetTkToken !PipeTkCallB)
  | ButtonNumber -> ev.Ev_ButtonNumber 	<- int_of_string (GetTkToken !PipeTkCallB)
  | Count 	-> ev.Ev_Count 		<- int_of_string (GetTkToken !PipeTkCallB)
  | Detail 	-> ev.Ev_Detail 	<- GetTkToken !PipeTkCallB
  | Focus 	-> ev.Ev_Focus 		<- GetTkToken !PipeTkCallB = "1"
  | Height 	-> ev.Ev_Height 	<- int_of_string (GetTkToken !PipeTkCallB)
  | KeyCode 	-> ev.Ev_KeyCode 	<- int_of_string (GetTkToken !PipeTkCallB)
  | Mode 	-> ev.Ev_Mode 		<- GetTkToken !PipeTkCallB
  | State 	-> ev.Ev_State 		<- GetTkToken !PipeTkCallB
  | ValueMask	-> ev.Ev_ValueMask 	<- int_of_string (GetTkToken !PipeTkCallB)
  | Width 	-> ev.Ev_Width 		<- int_of_string (GetTkToken !PipeTkCallB)
  | MouseX 	-> ev.Ev_MouseX 	<- int_of_string (GetTkToken !PipeTkCallB)
  | MouseY 	-> ev.Ev_MouseY 	<- int_of_string (GetTkToken !PipeTkCallB)
  | WidgetName 	-> ev.Ev_WidgetName 	<- GetTkToken !PipeTkCallB
  | Time 	-> ev.Ev_Time  		<- GetTkToken !PipeTkCallB
  | OverrideRedirect -> ev.Ev_OverrideRedirect <- GetTkToken !PipeTkCallB
  | Place 	-> ev.Ev_Place 		<- GetTkToken !PipeTkCallB
  | Char 	-> ev.Ev_Char 		<- GetTkString !PipeTkCallB
  | BorderWidth -> ev.Ev_BorderWidth 	<- int_of_string (GetTkToken !PipeTkCallB)
  | Display 	-> ev.Ev_Display 	<- int_of_string (GetTkToken !PipeTkCallB)
  | SendEvent 	-> ev.Ev_SendEvent 	<- int_of_string (GetTkToken !PipeTkCallB)
  | KeySymString -> ev.Ev_KeySymString 	<- GetTkToken !PipeTkCallB
  | KeySymInt 	-> ev.Ev_KeySymInt 	<- int_of_string (GetTkToken !PipeTkCallB)
  | RootWindow 	-> ev.Ev_RootWindow 	<- int_of_string (GetTkToken !PipeTkCallB)
  | SubWindow 	-> ev.Ev_SubWindow 	<- int_of_string (GetTkToken !PipeTkCallB)
  | Type 	-> ev.Ev_Type 		<- int_of_string (GetTkToken !PipeTkCallB)
  | RootX 	-> ev.Ev_RootX 		<- int_of_string (GetTkToken !PipeTkCallB)
  | RootY 	-> ev.Ev_RootY 		<- int_of_string (GetTkToken !PipeTkCallB);;


let WrapEventInfo f what =
  let ev = {
    Ev_Above = 0;
    Ev_ButtonNumber = 0;
    Ev_Count = 0;
    Ev_Detail = "";
    Ev_Focus = false;
    Ev_Height = 0;
    Ev_KeyCode = 0;
    Ev_Mode = "";
    Ev_State = "";
    Ev_ValueMask = 0;
    Ev_Width = 0;
    Ev_MouseX = 0;
    Ev_MouseY = 0;
    Ev_WidgetName = "";
    Ev_Time = "";
    Ev_OverrideRedirect = "";
    Ev_Place = "";
    Ev_Char = "";
    Ev_BorderWidth = 0;
    Ev_Display = 0;
    Ev_SendEvent = 0;
    Ev_KeySymString = "";
    Ev_KeySymInt = 0;
    Ev_RootWindow = 0;
    Ev_SubWindow = 0;
    Ev_Type = 0;
    Ev_RootX = 0;
    Ev_RootY = 0 } in
     function () ->
       do_list (FillEventInfo ev) what;
       f ev
;;



let rec WriteEventField = function
    [] -> ""
  | H::q ->
    begin
    match H with
      Above -> "puts $PipeTkCallB %a;"
      | ButtonNumber -> "puts $PipeTkCallB %b;"
      | Count -> "puts $PipeTkCallB %c;"
      | Detail -> "puts $PipeTkCallB %d;"
      | Focus -> "puts $PipeTkCallB %f;"
      | Height -> "puts $PipeTkCallB %h;"
      | KeyCode -> "puts $PipeTkCallB %k;"
      | Mode -> "puts $PipeTkCallB %m;"
      | State -> "puts $PipeTkCallB %s;"
      | ValueMask -> "puts $PipeTkCallB %v;"
      | Width -> "puts $PipeTkCallB %w;"
      | MouseX -> "puts $PipeTkCallB %x;"
      | MouseY -> "puts $PipeTkCallB %y;"
      | WidgetName -> "puts $PipeTkCallB %W;"
      | Time -> "puts $PipeTkCallB %t;"
      | OverrideRedirect -> "puts $PipeTkCallB %o;"
      | Place -> "puts $PipeTkCallB %p;"
      | Char -> "nputs $PipeTkCallB %A;"
      | BorderWidth -> "puts $PipeTkCallB %B;"
      | Display -> "puts $PipeTkCallB %D;"
      | SendEvent -> "puts $PipeTkCallB %E;"
      | KeySymString -> "puts $PipeTkCallB %K;"
      | KeySymInt -> "puts $PipeTkCallB %N;"
      | RootWindow -> "puts $PipeTkCallB %R;"
      | SubWindow -> "puts $PipeTkCallB %S;"
      | Type -> "puts $PipeTkCallB %T;"
      | RootX -> "puts $PipeTkCallB %X;"
      | RootY -> "puts $PipeTkCallB %Y;"
    end 
    ^ WriteEventField q;;


type BindAction =
   BindSet of EventField list *  (EventInfo -> unit)
 | BindRemove
;;


(* bind: Widget -> (Modifier list * XEvent) list -> BindAction -> unit *)

let bind widget eventsequence action =
  Send2TkStart "$PipeTkCallB";
  Send2Tk ("bind "^ widget_name widget^ " " ^ (CAMLtoTKEventSequence eventsequence));
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

