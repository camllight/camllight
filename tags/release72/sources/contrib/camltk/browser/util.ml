(* Miscellaneous utilities *)
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

(********************* Navigation for Text widgets *******************)
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
   (* and the bindings will not be activated *)
   (* therefore, do it manually *)
   bind tx [[Any],Enter] (BindSet ([],fun _ -> focus__set tx));
   bind tx [[Any],Leave] (BindSet ([],fun _ -> focus__none ()));
   page_down_keys tx sb;
   page_up_keys tx sb;
   line_down_keys tx sb;
   line_up_keys tx sb
;;

(********************* Line editor for Entry widgets *********************)
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

(* Could you bind_class Entry instead *)
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

(**************************** Simple requester ********************)
(* Note: grabs focus, thus always unique at one given moment      *)

let open_req title action memory =
  let t = toplevelw__create (support__new_toplevel_widget "open") [] in
  focus__set t;
  grab__set_local t;
  let tit = label__create t [Text title] in
  let e = entry__create t [Relief Sunken; TextVariable memory] in
    util__entry_bindings e;

  let activate _ =
      grab__release t;
      action (entry__get e);
      destroy t in
  
    bind e [[], XKey "Return"] (BindSet ([], activate));

  let f = frame__create t [] in
  let bok = button__create f [Text "Ok"; Command activate] in
  let bcancel = button__create f
      	    [Text "Cancel"; 
      	     Command (fun () -> destroy t)] in

    bind t [[], XKey "Return"]
      	 (BindSet ([], (fun _ -> button__invoke bok)));
    bind t [[], XKey "Escape"]
      	 (BindSet ([], (fun _ -> button__invoke bcancel)));
    bind e [[], XKey "Escape"]
      	 (BindSet ([], (fun _ -> button__invoke bcancel)));
    pack [bok; bcancel] [Side Side_Left; Fill Fill_X; Expand true];
    pack [tit;e] [Fill Fill_X];
    pack [f] [Side Side_Bottom; Fill Fill_X];
    util__resizeable t;
    focus__set e
;;

(******************** Simple list requester ********************)
let open_list_req title elements action =
  let t = toplevelw__create (support__new_toplevel_widget "openlist") [] in
  update_idletasks();
  grab__set_local t;
  let tit = label__create t [Text title] in

  let fls = frame__create t [Relief Sunken; Borderwidth (Pixels 2)] in 
  let lb = listbox__create fls [] in
  let sb = scrollbar__create fls [] in
    scroll_listbox_link sb lb;
    listbox__insert lb End elements;
  let activate _ =
    grab__release t;
    do_list (fun s -> action (listbox__get lb s))
      	    (listbox__curselection lb);
    destroy t  in

  bind lb [[Double], WhatButton 1] (BindSet ([], activate));
  bind lb [[Meta], XKey "a"] 
      (BindSet([], function _ ->
      	       	     listbox__select_from lb (Number 0);
		     listbox__select_to lb End));

  complete__add_completion lb activate; 

  let f = frame__create t [] in
  let bok = button__create f [Text "Ok"; Command activate] in
  let bcancel = button__create f 
      	       	      [Text "Cancel"; Command (fun () -> destroy t)] in

    pack [bok; bcancel] [Side Side_Left; Fill Fill_X; Expand true];
    pack [lb] [Side Side_Left; Fill Fill_Both; Expand true];
    pack [sb] [Side Side_Right; Fill Fill_Y];
    pack [tit] [Fill Fill_X];
    pack [fls] [Fill Fill_Both; Expand true];
    pack [f] [Side Side_Bottom; Fill Fill_X];
    util__resizeable t
;;
