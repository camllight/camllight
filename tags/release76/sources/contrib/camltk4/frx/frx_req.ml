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

let open_simple title action notaction memory =
  let t = toplevelw__create support__default_toplevel_widget [Class "Dialog"] in
  focus__set t;
  wm__title_set t title;
  let tit = label__create t [Text title] in
  let len = max 40 (string_length (textvariable__get memory)) in
  let e =
    entry__create t [Relief Sunken; TextVariable memory; TextWidth len] in

  let activate _ =
    let v = entry__get e in
     grab__release t;			(* because of wm *)
     destroy t;				(* so action can call open_simple *)
     action v in

  bind e [[], KeyPressDetail "Return"] (BindSet ([], activate));

  let f = frame__create t [] in
  let bok = button__create f [Text "Ok"; Command activate] in
  let bcancel = button__create f
      	    [Text "Cancel"; 
             Command (fun () -> notaction(); grab__release t; destroy t)] in

    bind e [[], KeyPressDetail "Escape"]
      	 (BindSet ([], (fun _ -> button__invoke bcancel)));
    pack [bok] [Side Side_Left; Expand true];
    pack [bcancel] [Side Side_Right; Expand true];
    pack [tit;e] [Fill Fill_X];
    pack [f] [Side Side_Bottom; Fill Fill_X];
    frx_widget__resizeable t;
    focus__set e;
    tkwait__visibility t;
    grab__set t
;;

(* A synchronous version *)
let open_simple_synchronous title memory =
  let t = toplevelw__create support__default_toplevel_widget [Class "Dialog"] in
  focus__set t;
  wm__title_set t title;
  let tit = label__create t [Text title] in
  let len = max 40 (string_length (textvariable__get memory)) in
  let e =
    entry__create t [Relief Sunken; TextVariable memory; TextWidth len] in

  let waiting = textvariable__create_temporary t in
  
  let activate _ =
     grab__release t;			(* because of wm *)
     destroy t;				(* so action can call open_simple *)
     textvariable__set waiting "1" in

  bind e [[], KeyPressDetail "Return"] (BindSet ([], activate));

  let f = frame__create t [] in
  let bok = button__create f [Text "Ok"; Command activate] in
  let bcancel = 
     button__create f
	[Text "Cancel"; 
	 Command (fun () -> 
		   grab__release t; destroy t; textvariable__set waiting "0")] in

    bind e [[], KeyPressDetail "Escape"]
      	 (BindSet ([], (fun _ -> button__invoke bcancel)));
    pack [bok] [Side Side_Left; Expand true];
    pack [bcancel] [Side Side_Right; Expand true];
    pack [tit;e] [Fill Fill_X];
    pack [f] [Side Side_Bottom; Fill Fill_X];
    frx_widget__resizeable t;
    focus__set e;
    tkwait__visibility t;
    grab__set t;
    tkwait__variable waiting;
    begin match textvariable__get waiting with
      "1" -> true
    | _ -> false
    end
;;
(*
 * Simple list requester
 * Same remarks as in open_simple.
 * focus seems to be in the listbox automatically
 *)
let open_list title elements action notaction =
  let t = toplevelw__create support__default_toplevel_widget [Class "Dialog"] in
  wm__title_set t title;

  let tit = label__create t [Text title] in
  let fls = frame__create t [Relief Sunken; BorderWidth (Pixels 2)] in 
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


  bind lb [[Double], ButtonPressDetail 1] (BindSetBreakable ([], activate));

  frx_listbox__add_completion lb activate; 

  let f = frame__create t [] in
  let bok = button__create f [Text "Ok"; Command activate] in
  let bcancel = button__create f 
      	    [Text "Cancel"; 
             Command (fun () -> notaction(); grab__release t; destroy t)] in

    pack [bok; bcancel] [Side Side_Left; Fill Fill_X; Expand true];
    pack [lb] [Side Side_Left; Fill Fill_Both; Expand true];
    pack [sb] [Side Side_Right; Fill Fill_Y];
    pack [tit] [Fill Fill_X];
    pack [fls] [Fill Fill_Both; Expand true];
    pack [f] [Side Side_Bottom; Fill Fill_X];
    frx_widget__resizeable t;
    tkwait__visibility t;
    grab__set t
;;

(* Synchronous *)
let open_passwd title =
  let username = ref ""
  and password = ref ""
  and cancelled = ref false in
  let t = toplevelw__create support__default_toplevel_widget [Class "Dialog"] in
  focus__set t;
  wm__title_set t title;
  let tit = label__create t [Text title]
  and fu,eu = frx_entry__new_label_entry t "Username" (fun s -> ())
  and fp,ep = frx_entry__new_label_entry t "Password" (fun s -> ())
  in
  let fb = frame__create t [] in
   let bok = button__create fb 
      	      [Text "Ok"; Command (fun _ -> 
	                            username := entry__get eu;
				    password := entry__get ep;
                                    grab__release t; (* because of wm *)
                                    destroy t)] (* will return from tkwait *)
   and bcancel = button__create fb
              [Text "Cancel"; Command (fun _ ->
	                            cancelled := true;
                                    grab__release t; (* because of wm *)
                                    destroy t)] (* will return from tkwait *)
  in
    entry__configure ep [Show `*`];
    bind eu [[], KeyPressDetail "Return"]
      (BindSetBreakable ([], (fun _ -> focus__set ep; break())));
    bind ep [[], KeyPressDetail "Return"]
      (BindSetBreakable ([], (fun _ -> button__flash bok; 
                                       button__invoke bok; 
      	       	       	       	       break())));

    pack [bok] [Side Side_Left; Expand true];
    pack [bcancel] [Side Side_Right; Expand true];
    pack [tit;fu;fp;fb] [Fill Fill_X];
    tkwait__visibility t;
    focus__set eu;
    grab__set t;
    tkwait__window t;
    if !cancelled then failwith "cancelled"
    else (!username, !password)
;;
