(* Events and bindings *)

(* Builtin types *)
(* type *)
type xEvent =
    ButtonPress (* also Button, but we omit it *)
  | ButtonPressDetail of int
  | ButtonRelease
  | ButtonReleaseDetail of int
  | Circulate
  | ColorMap
  | Configure
  | Destroy
  | Enter
  | Expose
  | FocusIn
  | FocusOut
  | Gravity
  | KeyPress (* also Key, but we omit it *)
  | KeyPressDetail of string      (* /usr/include/X11/keysymdef.h *)
  | KeyRelease
  | KeyReleaseDetail of string
  | Leave
  | Map
  | Motion
  | Property
  | Reparent
  | Unmap
  | Visibility 
;;
(* /type *)

let cCAMLtoTKxEvent = function
    ButtonPress -> "ButtonPress"
  | ButtonPressDetail n -> "ButtonPress-"^string_of_int n
  | ButtonRelease -> "ButtonRelease"
  | ButtonReleaseDetail n -> "ButtonRelease-"^string_of_int n
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
  | KeyPressDetail s -> "KeyPress-"^s
  | KeyRelease -> "KeyRelease"
  | KeyReleaseDetail s -> "KeyRelease-"^s
  | Leave -> "Leave"
  | Map -> "Map"
  | Motion -> "Motion"
  | Property -> "Property"
  | Reparent -> "Reparent"
  | Unmap -> "Unmap"
  | Visibility -> "Visibility" 
;;

(* type *)
type modifier =
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
(* /type *)

let cCAMLtoTKmodifier = function
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


(* type event = modifier list * xEvent *)
let cCAMLtoTKevent (ml, xe) =
  "<" ^ (catenate_sep " " (map cCAMLtoTKmodifier ml))  
      ^ (cCAMLtoTKxEvent xe) ^ ">"
;;
  
(* type eventSequence == (modifier list * xEvent) list *)
let cCAMLtoTKeventSequence l =
  TkToken(it_list (prefix ^) "" (map cCAMLtoTKevent l))
;;

(* Event structure, passed to bounded functions *)

(* type *)
type eventInfo =
  {
  mutable ev_Above : int;               (* tk: %a *)
  mutable ev_ButtonNumber : int;        (* tk: %b *)
  mutable ev_Count : int;               (* tk: %c *)
  mutable ev_Detail : string;           (* tk: %d *)
  mutable ev_Focus : bool;              (* tk: %f *)
  mutable ev_Height : int;              (* tk: %h *)
  mutable ev_KeyCode : int;             (* tk: %k *)
  mutable ev_Mode : string;             (* tk: %m *)
  mutable ev_OverrideRedirect : bool;   (* tk: %o *)
  mutable ev_Place : string;            (* tk: %p *)
  mutable ev_State : string;            (* tk: %s *)
  mutable ev_Time : int;                (* tk: %t *)
  mutable ev_Width : int;               (* tk: %w *)
  mutable ev_MouseX : int;              (* tk: %x *)
  mutable ev_MouseY : int;              (* tk: %y *)
  mutable ev_Char : string;             (* tk: %A *)
  mutable ev_BorderWidth : int;         (* tk: %B *)
  mutable ev_SendEvent : bool;          (* tk: %E *)
  mutable ev_KeySymString : string;     (* tk: %K *)
  mutable ev_KeySymInt : int;           (* tk: %N *)
  mutable ev_RootWindow : int;          (* tk: %R *)
  mutable ev_SubWindow : int;           (* tk: %S *)
  mutable ev_Type : int;                (* tk: %T *)
  mutable ev_Widget : widget;           (* tk: %W *)
  mutable ev_RootX : int;               (* tk: %X *)
  mutable ev_RootY : int                (* tk: %Y *)
  }
;;
(* /type *)


(* To avoid collision with other constructors (Width, State), 
   use Ev_ prefix *)
(* type *)
type eventField =
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
(* /type *)

let filleventInfo ev v = function 
    Ev_Above    -> 	ev.ev_Above <- int_of_string v
  | Ev_ButtonNumber -> 	ev.ev_ButtonNumber <- int_of_string v
  | Ev_Count -> 	ev.ev_Count <- int_of_string v
  | Ev_Detail -> 	ev.ev_Detail <- v
  | Ev_Focus -> 	ev.ev_Focus <- v = "1"
  | Ev_Height -> 	ev.ev_Height <- int_of_string v
  | Ev_KeyCode -> 	ev.ev_KeyCode <- int_of_string v
  | Ev_Mode -> 		ev.ev_Mode <- v
  | Ev_OverrideRedirect -> ev.ev_OverrideRedirect <- v = "1"
  | Ev_Place -> 	ev.ev_Place <- v
  | Ev_State -> 	ev.ev_State <- v
  | Ev_Time -> 		ev.ev_Time <- int_of_string v
  | Ev_Width -> 	ev.ev_Width <- int_of_string v
  | Ev_MouseX -> 	ev.ev_MouseX <- int_of_string v
  | Ev_MouseY -> 	ev.ev_MouseY <- int_of_string v
  | Ev_Char -> 		ev.ev_Char <- v
  | Ev_BorderWidth -> 	ev.ev_BorderWidth <- int_of_string v
  | Ev_SendEvent -> 	ev.ev_SendEvent <- v = "1"
  | Ev_KeySymString -> 	ev.ev_KeySymString <- v
  | Ev_KeySymInt -> 	ev.ev_KeySymInt <- int_of_string v
  | Ev_RootWindow -> 	ev.ev_RootWindow <- int_of_string v
  | Ev_SubWindow -> 	ev.ev_SubWindow <- int_of_string v
  | Ev_Type -> 		ev.ev_Type <- int_of_string v
  | Ev_Widget -> 	ev.ev_Widget <- cTKtoCAMLwidget v
  | Ev_RootX -> 	ev.ev_RootX <- int_of_string v
  | Ev_RootY -> 	ev.ev_RootY <- int_of_string v
;;

let wrapeventInfo f what =
  let ev = {
    ev_Above = 0;
    ev_ButtonNumber = 0;
    ev_Count = 0;
    ev_Detail = "";
    ev_Focus = false;
    ev_Height = 0;
    ev_KeyCode = 0;
    ev_Mode = "";
    ev_OverrideRedirect = false;
    ev_Place = "";
    ev_State = "";
    ev_Time = 0;
    ev_Width = 0;
    ev_MouseX = 0;
    ev_MouseY = 0;
    ev_Char = "";
    ev_BorderWidth = 0;
    ev_SendEvent = false;
    ev_KeySymString = "";
    ev_KeySymInt = 0;
    ev_RootWindow = 0;
    ev_SubWindow = 0;
    ev_Type = 0;
    ev_Widget = default_toplevel_widget;
    ev_RootX = 0;
    ev_RootY = 0 } in
     function args ->
       let l = ref args in
         do_list (function field ->
	            match !l with
		      [] -> ()
		    | v::rest -> filleventInfo ev v field; l:=rest)
                 what;
       f ev
;;



let rec writeeventField = function
    [] -> ""
  | field::rest ->
    begin
    match field with
        Ev_Above ->     " %a"
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
    ^ writeeventField rest
;;


(* type *)
type bindAction =
   BindSet of eventField list *  (eventInfo -> unit)
 | BindSetBreakable of eventField list *  (eventInfo -> unit)
 | BindRemove
 | BindExtend of eventField list *  (eventInfo -> unit)
;;
(* /type *)

(*
FUNCTION
 val bind: 
    widget -> (modifier list * xEvent) list -> bindAction -> unit
/FUNCTION
*)
let bind widget eventsequence action =
  tkDo [| TkToken "bind";
      	    TkToken (widget_name widget);
	    cCAMLtoTKeventSequence eventsequence;
  begin match action with
     BindRemove -> TkToken ""
  |  BindSet (what, f) ->
      let cbId = register_callback widget (wrapeventInfo f what) in
        TkToken ("camlcb " ^ cbId ^ (writeeventField what))
  |  BindSetBreakable (what, f) ->
      let cbId = register_callback widget (wrapeventInfo f what) in
        TkToken ("camlcb " ^ cbId ^ (writeeventField what)^
                 " ; if { $BreakBindingsSequence == 1 } then { break ;} ; set BreakBindingsSequence 0"
                )
  |  BindExtend (what, f) ->
      let cbId = register_callback widget (wrapeventInfo f what) in
        TkToken ("+camlcb " ^ cbId ^ (writeeventField what))
      
  end
  |];
  ()
;;

(* 
FUNCTION
(* unsafe *)
 val class_bind : 
    string -> (modifier list * xEvent) list -> bindAction -> unit 
(* /unsafe *)
/FUNCTION
 class arg is not constrained
*)
let class_bind clas eventsequence action =
  tkDo [| TkToken "bind";
      	    TkToken clas;
	    cCAMLtoTKeventSequence eventsequence;
  begin match action with
     BindRemove -> TkToken ""
  |  BindSet (what, f) ->
      let cbId = register_callback dummy_widget (wrapeventInfo f what) in
        TkToken ("camlcb " ^ cbId ^ (writeeventField what))
  |  BindSetBreakable (what, f) ->
      let cbId = register_callback dummy_widget (wrapeventInfo f what) in
        TkToken ("camlcb " ^ cbId ^ (writeeventField what)^
                 " ; if { $BreakBindingsSequence == 1 } then { break ;} ; set BreakBindingsSequence 0"
                )
  |  BindExtend (what, f) ->
      let cbId = register_callback dummy_widget (wrapeventInfo f what) in
        TkToken ("+camlcb " ^ cbId ^ (writeeventField what))
      
  end
 |];
  ()
;;

(* 
FUNCTION
(* unsafe *)
 val tag_bind : 
    string -> (modifier list * xEvent) list -> bindAction -> unit 
(* /unsafe *)
/FUNCTION
 tag name arg is not constrained 
*)

let tag_bind = class_bind
;;


(*
FUNCTION
  val break : unit -> unit
/FUNCTION
*)
let break = function () ->
  tkDo [| TkToken "set" ; TkToken "BreakBindingsSequence" ; TkToken "1" |]
;;
