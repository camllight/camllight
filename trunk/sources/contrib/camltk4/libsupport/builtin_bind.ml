(* Events and bindings *)

(* Builtin types *)
type XEvent =
    XKey of string      (* /usr/include/X11/keysymdef.h *)
  | ButtonPress
  | Button
  | ButtonRelease
  | Circulate
  | ColorMap
  | Configure
  | Destroy
  | Enter
  | Expose
  | FocusIn
  | FocusOut
  | Gravity
  | KeyPress
  | Key
  | KeyRelease
  | Leave
  | Map
  | Motion
  | Property
  | Reparent
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
  | ColorMap -> "ColorMap"
  | Configure -> "Configure"
  | Destroy -> "Destroy"
  | Enter -> "Enter"
  | Expose -> "Expose"
  | FocusIn -> "FocusIn"
  | FocusOut -> "FocusOut"
  | Gravity -> "Gravity"
  | KeyPress -> "KeyPress"
  | Key -> "Key"
  | KeyRelease -> "KeyRelease"
  | Leave -> "Leave"
  | Map -> "Map"
  | Motion -> "Motion"
  | Property -> "Property"
  | Reparent -> "Reparent"
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
  | Double
  | Triple
  | Mod1
  | Mod2
  | Mod3
  | Mod4
  | Mod5
  | Meta
  | Alt 
;;

let CAMLtoTKModifier = function
   Control -> "Control-"
 | Shift -> "Shift-"
 | Lock -> "Lock-"
 | Button1 -> "Button1-"
 | Button2 -> "Button2-"
 | Button3 -> "Button3-"
 | Button4 -> "Button4-"
 | Button5 -> "Button5-"
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
  TkToken(it_list (prefix ^) "" (map CAMLtoTKEvent l))
;;

(* Event structure, passed to bounded functions *)

type EventInfo = {
  mutable Ev_Above : int;               (* tk: %a *)
  mutable Ev_ButtonNumber : int;        (* tk: %b *)
  mutable Ev_Count : int;               (* tk: %c *)
  mutable Ev_Detail : string;           (* tk: %d *)
  mutable Ev_Focus : bool;              (* tk: %f *)
  mutable Ev_Height : int;              (* tk: %h *)
  mutable Ev_KeyCode : int;             (* tk: %k *)
  mutable Ev_Mode : string;             (* tk: %m *)
  mutable Ev_OverrideRedirect : bool;   (* tk: %o *)
  mutable Ev_Place : string;            (* tk: %p *)
  mutable Ev_State : string;            (* tk: %s *)
  mutable Ev_Time : int;                (* tk: %t *)
  mutable Ev_Width : int;               (* tk: %w *)
  mutable Ev_MouseX : int;              (* tk: %x *)
  mutable Ev_MouseY : int;              (* tk: %y *)
  mutable Ev_Char : string;             (* tk: %A *)
  mutable Ev_BorderWidth : int;         (* tk: %B *)
  mutable Ev_SendEvent : bool;          (* tk: %E *)
  mutable Ev_KeySymString : string;     (* tk: %K *)
  mutable Ev_KeySymInt : int;           (* tk: %N *)
  mutable Ev_RootWindow : int;          (* tk: %R *)
  mutable Ev_SubWindow : int;           (* tk: %S *)
  mutable Ev_Type : int;                (* tk: %T *)
  mutable Ev_Widget : Widget;           (* tk: %W *)
  mutable Ev_RootX : int;               (* tk: %X *)
  mutable Ev_RootY : int                (* tk: %Y *)
  }
;;


(* To avoid collision with other constructors (Width, State), 
   use Ev_ prefix *)
type EventField =
    Ev_Above
  | Ev_ButtonNumber
  | Ev_Count
  | Ev_Detail
  | Ev_Focus
  | Ev_Height
  | Ev_KeyCode
  | Ev_Mode
  | Ev_OverrideRedirect
  | Ev_Place
  | Ev_State
  | Ev_Time 
  | Ev_Width
  | Ev_MouseX
  | Ev_MouseY
  | Ev_Char
  | Ev_BorderWidth
  | Ev_SendEvent
  | Ev_KeySymString
  | Ev_KeySymInt
  | Ev_RootWindow
  | Ev_SubWindow
  | Ev_Type
  | Ev_Widget
  | Ev_RootX
  | Ev_RootY
;;

let FillEventInfo ev v = function 
    Ev_Above    -> 	ev.Ev_Above <- int_of_string v
  | Ev_ButtonNumber -> 	ev.Ev_ButtonNumber <- int_of_string v
  | Ev_Count -> 	ev.Ev_Count <- int_of_string v
  | Ev_Detail -> 	ev.Ev_Detail <- v
  | Ev_Focus -> 	ev.Ev_Focus <- v = "1"
  | Ev_Height -> 	ev.Ev_Height <- int_of_string v
  | Ev_KeyCode -> 	ev.Ev_KeyCode <- int_of_string v
  | Ev_Mode -> 		ev.Ev_Mode <- v
  | Ev_OverrideRedirect -> ev.Ev_OverrideRedirect <- v = "1"
  | Ev_Place -> 	ev.Ev_Place <- v
  | Ev_State -> 	ev.Ev_State <- v
  | Ev_Time -> 		ev.Ev_Time <- int_of_string v
  | Ev_Width -> 	ev.Ev_Width <- int_of_string v
  | Ev_MouseX -> 	ev.Ev_MouseX <- int_of_string v
  | Ev_MouseY -> 	ev.Ev_MouseY <- int_of_string v
  | Ev_Char -> 		ev.Ev_Char <- v
  | Ev_BorderWidth -> 	ev.Ev_BorderWidth <- int_of_string v
  | Ev_SendEvent -> 	ev.Ev_SendEvent <- v = "1"
  | Ev_KeySymString -> 	ev.Ev_KeySymString <- v
  | Ev_KeySymInt -> 	ev.Ev_KeySymInt <- int_of_string v
  | Ev_RootWindow -> 	ev.Ev_RootWindow <- int_of_string v
  | Ev_SubWindow -> 	ev.Ev_SubWindow <- int_of_string v
  | Ev_Type -> 		ev.Ev_Type <- int_of_string v
  | Ev_Widget -> 	ev.Ev_Widget <- TKtoCAMLWidget v
  | Ev_RootX -> 	ev.Ev_RootX <- int_of_string v
  | Ev_RootY -> 	ev.Ev_RootY <- int_of_string v
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
    Ev_OverrideRedirect = false;
    Ev_Place = "";
    Ev_State = "";
    Ev_Time = 0;
    Ev_Width = 0;
    Ev_MouseX = 0;
    Ev_MouseY = 0;
    Ev_Char = "";
    Ev_BorderWidth = 0;
    Ev_SendEvent = false;
    Ev_KeySymString = "";
    Ev_KeySymInt = 0;
    Ev_RootWindow = 0;
    Ev_SubWindow = 0;
    Ev_Type = 0;
    Ev_Widget = default_toplevel_widget;
    Ev_RootX = 0;
    Ev_RootY = 0 } in
     function args ->
       let l = ref args in
         do_list (function field ->
	            match !l with
		      [] -> ()
		    | v::rest -> FillEventInfo ev v field; l:=rest)
                 what;
       f ev
;;



let rec WriteEventField = function
    [] -> ""
  | field::rest ->
    begin
    match field with
      | Ev_Above ->     " %a"
      | Ev_ButtonNumber ->" %b"
      | Ev_Count ->     " %c"
      | Ev_Detail ->    " %d"
      | Ev_Focus ->     " %f"
      | Ev_Height ->    " %h"
      | Ev_KeyCode ->   " %k"
      | Ev_Mode ->      " %m"
      | Ev_OverrideRedirect -> " %o"
      | Ev_Place ->     " %p"
      | Ev_State ->     " %s"
      | Ev_Time ->      " %t"
      | Ev_Width ->     " %w"
      | Ev_MouseX ->    " %x"
      | Ev_MouseY ->    " %y"
      (* Quoting is done by Tk *)
      | Ev_Char ->      " %A"
      | Ev_BorderWidth -> " %B"
      | Ev_SendEvent -> " %E"
      | Ev_KeySymString -> " %K"
      | Ev_KeySymInt -> " %N"
      | Ev_RootWindow ->" %R"
      | Ev_SubWindow -> " %S"
      | Ev_Type ->      " %T"
      | Ev_Widget ->" %W"
      | Ev_RootX ->     " %X"
      | Ev_RootY ->     " %Y"
    end 
    ^ WriteEventField rest;;


type BindAction =
   BindSet of EventField list *  (EventInfo -> unit)
 | BindSetBreakable of EventField list *  (EventInfo -> unit)
 | BindRemove
 | BindExtend of EventField list *  (EventInfo -> unit)
;;


(* bind: Widget -> (Modifier list * XEvent) list -> BindAction -> unit *)

let bind widget eventsequence action =
  TkEval [| TkToken "bind";
      	    TkToken (widget_name widget);
	    CAMLtoTKEventSequence eventsequence;
  begin match action with
     BindRemove -> TkToken ""
  |  BindSet (what, f) ->
      let CbId = register_callback widget (WrapEventInfo f what) in
        TkToken ("camlcb " ^ CbId ^ (WriteEventField what))
  |  BindSetBreakable (what, f) ->
      let CbId = register_callback widget (WrapEventInfo f what) in
        TkToken ("camlcb " ^ CbId ^ (WriteEventField what)^
                 " ; if { $BreakBindingsSequence == 1 } then { break ;} ; set BreakBindingsSequence 0"
                )
  |  BindExtend (what, f) ->
      let CbId = register_callback widget (WrapEventInfo f what) in
        TkToken ("+camlcb " ^ CbId ^ (WriteEventField what))
      
  end
  |];
  ()
;;

(* class_bind : string -> (Modifier list * XEvent) list -> BindAction -> unit 
      class arg is not constrained *)
let class_bind class eventsequence action =
  TkEval [| TkToken "bind";
      	    TkToken class;
	    CAMLtoTKEventSequence eventsequence;
  begin match action with
     BindRemove -> TkToken ""
  |  BindSet (what, f) ->
      let CbId = register_callback dummy_widget (WrapEventInfo f what) in
        TkToken ("camlcb " ^ CbId ^ (WriteEventField what))
  |  BindSetBreakable (what, f) ->
      let CbId = register_callback dummy_widget (WrapEventInfo f what) in
        TkToken ("camlcb " ^ CbId ^ (WriteEventField what)^
                 " ; if { $BreakBindingsSequence == 1 } then { break ;} ; set BreakBindingsSequence 0"
                )
  |  BindExtend (what, f) ->
      let CbId = register_callback dummy_widget (WrapEventInfo f what) in
        TkToken ("+camlcb " ^ CbId ^ (WriteEventField what))
      
  end
 |];
  ()
;;

(* tag_bind : string -> (Modifier list * XEvent) list -> BindAction -> unit 
      tag name arg is not constrained *)
let tag_bind = class_bind
;;


(* break : unit -> unit *)
let break = function () ->
  TkEval [| TkToken "set" ; TkToken "BreakBindingsSequence" ; TkToken "1" |];
  ()
;;
