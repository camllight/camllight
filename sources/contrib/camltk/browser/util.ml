(* Utils *)
#open "tk";;

(* Make a window (toplevel widget) resizeable *)
let resizeable t =
  update_idletasks(); (* wait until layout is computed *)
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

(********************* Line editor for entry widgets *********************)
(* tk 3.6 has standard bindings
   - (mouse selection)
   - Delete/BackSpace/Control-h	: backward-delete-char
   - Control-d			: kill-region
   - Control-u			: clear entry
   - Control-v			: paste
   - Control-w			: backward-kill-word
*)

let entry_backward_char e _ = 
  entry__icursor e (Number (pred (entry__index e Insert)));
  entry_see_caret e;
;;
let entry_forward_char e _ = 
  entry__icursor e (Number (succ (entry__index e Insert)));
  entry_see_caret e;
;;
let entry_bol e _ =
  entry__icursor e (Number 0);
  entry_see_caret e;
;;
let entry_eol e _ =
  entry__icursor e End;
  entry_see_caret e;
;;
let entry_kill_line e _ =
  entry__delete e Insert End;
  entry_see_caret e;
;;
let entry_delete_char e _ =
  entry__delete e Insert Insert;
  entry_see_caret e;
;;
let entry_kill_region e _ =
  try
    entry__delete e SelFirst SelLast;
    entry_see_caret e;
  with
   protocol__TkError "selection isn't in entry" -> ()
;;


let entry_bindings e =
  (* paste with mouse (already on C-v) *)
  bind e [[], WhatButton 2]
      	 (BindSet ([], (fun _ -> 
      	       	       	 try
      	       	       	   let s = selection__get() in
      	       	       	     entry__insert e Insert s
			 with _ -> ())));
  bind e [[Control], XKey "b"] (BindSet ([], entry_backward_char e));
  bind e [[], XKey "Left"] (BindSet ([], entry_backward_char e));
  bind e [[Control], XKey "f"] (BindSet ([], entry_forward_char e));
  bind e [[], XKey "Right"] (BindSet ([], entry_forward_char e));
  bind e [[Control], XKey "a"] (BindSet ([], entry_bol e));
  bind e [[], XKey "Home"] (BindSet ([], entry_bol e));
  bind e [[Control], XKey "e"] (BindSet ([], entry_eol e));
  bind e [[], XKey "End"] (BindSet ([], entry_eol e));
  bind e [[Control], XKey "k"] (BindSet ([], entry_kill_line e));
  (* Remove older bindings *)
  bind e [[Control], XKey "d"] BindRemove;
  bind e [[Control], XKey "w"] BindRemove;
  bind e [[Control], XKey "d"] (BindSet ([], entry_delete_char e));
  bind e [[Control], XKey "w"] (BindSet ([], entry_kill_region e));
;;
