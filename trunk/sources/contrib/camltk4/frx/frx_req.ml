#open "tk";;

(*
 * Some standard requesters (in Amiga techspeak) or dialog boxes (in Apple 
 * jargon).
*)

let version = "$Id$"
;;

(*
 * Simple requester 
 *  an entry field, unrestricted, with emacs-like bindings
 * Note: grabs focus, thus always unique at one given moment, and we
 *  shouldn't have to worry about toplevel widget name.
 * We add a title widget in case the window manager does not decorate
 * toplevel windows.
*)

let open_simple title action memory =
  let t = toplevelw__create (support__new_toplevel_widget "simpleReq") [] in
  focus__set t;
  grab__set_local t;
  wm__title_set t title;
  let tit = label__create t [Text title] in
  let e = entry__create t [Relief Sunken; TextVariable memory] in

  let activate _ =
    let v = entry__get e in
     grab__release t;			(* because of wm *)
     destroy t;				(* so action can call open_simple *)
     action v in

  bind e [[], XKey "Return"] (BindSet ([], activate));

  let f = frame__create t [] in
  let bok = button__create f [Text "Ok"; Command activate] in
  let bcancel = button__create f
      	    [Text "Cancel"; Command (fun () -> grab__release t; destroy t)] in

    bind t [[], XKey "Return"]
      	 (BindSet ([], (fun _ -> button__invoke bok)));
    bind t [[], XKey "Escape"]
      	 (BindSet ([], (fun _ -> button__invoke bcancel)));
    bind e [[], XKey "Escape"]
      	 (BindSet ([], (fun _ -> button__invoke bcancel)));
    pack [bok; bcancel] [Side Side_Left; Fill Fill_X; Expand true];
    pack [tit;e] [Fill Fill_X];
    pack [f] [Side Side_Bottom; Fill Fill_X];
    frx_widget__resizeable t;
    focus__set e
;;

(*
 * Simple list requester
 * Same remarks as in open_simple.
 * focus seems to be in the listbox automatically
 *)
let open_list title elements action =
  let t = toplevelw__create (support__new_toplevel_widget "openlist") [] in
  grab__set_local t;
  wm__title_set t title;

  let tit = label__create t [Text title] in
  let fls = frame__create t [Relief Sunken; Borderwidth (Pixels 2)] in 
  let lb = listbox__create fls [SelectMode Extended] in
  let sb = scrollbar__create fls [] in
    frx_listbox__scroll_link sb lb;
    listbox__insert lb End elements;

  (* activation: we have to break() because we destroy the requester *)
  let activate _ =
    let l = map (listbox__get lb) (listbox__curselection lb) in
    grab__release t;
    destroy t;
    do_list action l;
    break() in


  bind lb [[Double], WhatButton 1] (BindSetBreakable ([], activate));

  frx_listbox__add_completion lb activate; 

  let f = frame__create t [] in
  let bok = button__create f [Text "Ok"; Command activate] in
  let bcancel = button__create f 
      	    [Text "Cancel"; Command (fun () -> grab__release t; destroy t)] in

    pack [bok; bcancel] [Side Side_Left; Fill Fill_X; Expand true];
    pack [lb] [Side Side_Left; Fill Fill_Both; Expand true];
    pack [sb] [Side Side_Right; Fill Fill_Y];
    pack [tit] [Fill Fill_X];
    pack [fls] [Fill Fill_Both; Expand true];
    pack [f] [Side Side_Bottom; Fill Fill_X];
    frx_widget__resizeable t
;;
