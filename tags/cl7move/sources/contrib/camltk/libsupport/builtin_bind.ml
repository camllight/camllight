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

let FillEventInfo ev args = function 
    Above 	-> ev.Ev_Above 		<- int_of_string (arg_GetTkToken args)
  | ButtonNumber -> ev.Ev_ButtonNumber 	<- int_of_string (arg_GetTkToken args)
  | Count 	-> ev.Ev_Count 		<- int_of_string (arg_GetTkToken args)
  | Detail 	-> ev.Ev_Detail 	<- arg_GetTkToken args
  | Focus 	-> ev.Ev_Focus 		<- arg_GetTkToken args = "1"
  | Height 	-> ev.Ev_Height 	<- int_of_string (arg_GetTkToken args)
  | KeyCode 	-> ev.Ev_KeyCode 	<- int_of_string (arg_GetTkToken args)
  | Mode 	-> ev.Ev_Mode 		<- arg_GetTkToken args
  | State 	-> ev.Ev_State 		<- arg_GetTkToken args
  | ValueMask	-> ev.Ev_ValueMask 	<- int_of_string (arg_GetTkToken args)
  | Width 	-> ev.Ev_Width 		<- int_of_string (arg_GetTkToken args)
  | MouseX 	-> ev.Ev_MouseX 	<- int_of_string (arg_GetTkToken args)
  | MouseY 	-> ev.Ev_MouseY 	<- int_of_string (arg_GetTkToken args)
  | WidgetName 	-> ev.Ev_WidgetName 	<- arg_GetTkToken args
  | Time 	-> ev.Ev_Time  		<- arg_GetTkToken args
  | OverrideRedirect -> ev.Ev_OverrideRedirect <- arg_GetTkToken args
  | Place 	-> ev.Ev_Place 		<- arg_GetTkToken args
  | Char 	-> ev.Ev_Char 		<- arg_GetTkToken args (* WARNING *)
  | BorderWidth -> ev.Ev_BorderWidth 	<- int_of_string (arg_GetTkToken args)
  | Display 	-> ev.Ev_Display 	<- int_of_string (arg_GetTkToken args)
  | SendEvent 	-> ev.Ev_SendEvent 	<- int_of_string (arg_GetTkToken args)
  | KeySymString -> ev.Ev_KeySymString 	<- arg_GetTkToken args
  | KeySymInt 	-> ev.Ev_KeySymInt 	<- int_of_string (arg_GetTkToken args)
  | RootWindow 	-> ev.Ev_RootWindow 	<- int_of_string (arg_GetTkToken args)
  | SubWindow 	-> ev.Ev_SubWindow 	<- int_of_string (arg_GetTkToken args)
  | Type 	-> ev.Ev_Type 		<- int_of_string (arg_GetTkToken args)
  | RootX 	-> ev.Ev_RootX 		<- int_of_string (arg_GetTkToken args)
  | RootY 	-> ev.Ev_RootY 		<- int_of_string (arg_GetTkToken args)
;;


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
     function args ->
       do_list (FillEventInfo ev args) what;
       f ev
;;



let rec WriteEventField = function
    [] -> ""
  | H::q ->
    begin
    match H with
        Above -> 	" %a"
      | ButtonNumber -> " %b"
      | Count -> 	" %c"
      | Detail -> 	" %d"
      | Focus -> 	" %f"
      | Height -> 	" %h"
      | KeyCode -> 	" %k"
      | Mode -> 	" %m"
      | State -> 	" %s"
      | ValueMask -> 	" %v"
      | Width -> 	" %w"
      | MouseX -> 	" %x"
      | MouseY -> 	" %y"
      | WidgetName -> 	" %W"
      | Time -> 	" %t"
      | OverrideRedirect -> " %o"
      | Place -> 	" %p"
      (* Quoting is done by Tk *)
      | Char -> 	" %A"
      | BorderWidth -> 	" %B"
      | Display -> 	" %D"
      | SendEvent -> 	" %E"
      | KeySymString -> " %K"
      | KeySymInt -> 	" %N"
      | RootWindow -> 	" %R"
      | SubWindow -> 	" %S"
      | Type -> 	" %T"
      | RootX -> 	" %X"
      | RootY -> 	" %Y"
    end 
    ^ WriteEventField q;;


type BindAction =
   BindSet of EventField list *  (EventInfo -> unit)
 | BindRemove
 | BindExtend of EventField list *  (EventInfo -> unit)
;;


(* bind: Widget -> (Modifier list * XEvent) list -> BindAction -> unit *)

let bind widget eventsequence action =
  let buf = Send2TkStart false in
  Send2Tk buf ("bind "^ widget_name widget^" "
      	              ^ (CAMLtoTKEventSequence eventsequence));
  begin match action with
     BindRemove -> Send2Tk buf "{}"
  |  BindSet (what, f) ->
      let CbId = register_callback (WrapEventInfo f what) in
        Send2Tk buf (" {camlcb " ^ CbId ^ (WriteEventField what) ^"}")
  |  BindExtend (what, f) ->
      let CbId = register_callback (WrapEventInfo f what) in
        Send2Tk buf (" {+camlcb " ^ CbId ^ (WriteEventField what) ^"}")
  end;
  Send2TkEval buf;
  ()
;;

(* class_bind : string -> (Modifier list * XEvent) list -> BindAction -> unit 
      class arg is not constrained *)
let class_bind class eventsequence action =
  let buf = Send2TkStart false in
  Send2Tk buf ("bind "^ class ^" "
      	              ^ (CAMLtoTKEventSequence eventsequence));
  begin match action with
     BindRemove -> Send2Tk buf "{}"
  |  BindSet (what, f) ->
      let CbId = register_callback (WrapEventInfo f what) in
        Send2Tk buf (" {camlcb " ^ CbId ^ (WriteEventField what) ^"}")
  |  BindExtend (what, f) ->
      let CbId = register_callback (WrapEventInfo f what) in
        Send2Tk buf (" {+camlcb " ^ CbId ^ (WriteEventField what) ^"}")
  end;
  Send2TkEval buf;
  ()
;;
