(* Utils *)
#open "tk";;

(* Make a window (toplevel widget) resizeable *)
let resizeable t =
  update(); (* wait until layout is computed *)
  wm__minsize_set t (winfo__width t) (winfo__height t)
;;

(* Link a scrollbar and a text widget *)
let scroll_text_link sb tx =
  text__configure tx [YScrollCommand (scrollbar__set sb)];
  scrollbar__configure sb [Slidecommand (text__yview_line tx)]
;;


(* Link a scrollbar and a listbox *)
let scroll_listbox_link sb lb =
  listbox__configure lb 
      	[YScrollCommand (scrollbar__set sb)];
  scrollbar__configure sb 
        [Slidecommand (fun n -> listbox__yview lb (Number n))]
;;

(* Text scrollers in Disabled mode : we can't use the insertion mark *)
(* MUST HAVE an associated scrollbar 				     *)
(* totalUnits = number of lines 
   windowUnits = number of visible lines
   firstUnit = first visible line
   lastUnit  = last visible line
*)
let page_up tx sb =
  let totalUnits,windowUnits,firstUnit,lastUnit =
      	 scrollbar__get sb in
   let newfirst = firstUnit - windowUnits in
     if newfirst >= 0 then text__yview_line tx newfirst
     else text__yview_line tx 0
;;
let page_down tx sb =
  let totalUnits,windowUnits,firstUnit,lastUnit =
      	 scrollbar__get sb in
   let newfirst = firstUnit + windowUnits in
     if newfirst <= totalUnits then text__yview_line tx newfirst
;;

let line_up tx sb = 
  let totalUnits,windowUnits,firstUnit,lastUnit =
      	 scrollbar__get sb in
   let newfirst = pred firstUnit in
     if newfirst >= 0 then text__yview_line tx newfirst
;;

let line_down tx sb =
  let totalUnits,windowUnits,firstUnit,lastUnit =
      	 scrollbar__get sb in
   let newfirst = succ firstUnit in
     if newfirst <= totalUnits then text__yview_line tx newfirst
;;


let page_up_keys tx sb =
  do_list (function ev ->
      	bind tx ev (BindSet([], (fun _ -> page_up tx sb))))
  [
   [[], XKey "BackSpace"];
   [[], XKey "Delete"];
   [[], XKey "Prior"];
   [[Meta], XKey "v"]
  ]
;;

let page_down_keys tx sb =
  do_list (function ev ->
      	bind tx ev (BindSet([], (fun _ -> page_down tx sb))))
  [
   [[], XKey "space"];
   [[], XKey "Next"];
   [[Control], XKey "v"]
  ]
;;

let line_up_keys tx sb =
  do_list (function ev ->
      	bind tx ev (BindSet([], (fun _ -> line_up tx sb))))
  [
   [[], XKey "Up"];
   [[Meta], XKey "z"]
  ]
;;

let line_down_keys tx sb =
  do_list (function ev ->
      	bind tx ev (BindSet([], (fun _ -> line_down tx sb))))
  [
   [[], XKey "Down"];
   [[Control], XKey "z"]
  ]
;;

let navigation_keys tx sb =
   (* if text widget is disabled, clicking in it will not set the focus *)
   (* and the bindings will not br activated *)
   (* therefore, do it manually *)
   bind tx [[Any],Enter] (BindSet ([],fun _ -> focus__set tx));
   bind tx [[Any],Leave] (BindSet ([],fun _ -> focus__none ()));
   page_down_keys tx sb;
   page_up_keys tx sb;
   line_down_keys tx sb;
   line_up_keys tx sb
;;
