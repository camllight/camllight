#open "tk";;

let version = "$Id$"
;;

(* 
 * convert an integer to an absolute index 
*)
let abs_index n =
  TextIndex (LineChar(0,0), [CharOffset n])
;;
let insertMark =
  TextIndex(Mark "insert", [])
;;
let currentMark =
  TextIndex(Mark "current", [])
;;
let textEnd =
  TextIndex(End, [])
;;
let textBegin =
  TextIndex (LineChar(0,0), [])
;;
(* 
 * Link a scrollbar and a text widget 
*)
let scroll_link sb tx =
  text__configure tx [YScrollCommand (scrollbar__set sb)];
  scrollbar__configure sb [ScrollCommand (text__yview tx)]
;;

(*
 * Tk 4.0 has navigation in Text widgets, sometimes using scrolling
 * sometimes using the insertion mark. It is a pain to add more
 * compatible bindings. We do our own.
 *)
let page_up tx   =  text__yview tx (ScrollPage (-1))
and page_down tx =  text__yview tx (ScrollPage 1)
and line_up tx   =  text__yview tx (ScrollUnit (-1))
and line_down tx =  text__yview tx (ScrollUnit 1)
and top tx = text__yview_index tx textBegin
and bottom tx = text__yview_index tx textEnd
;;
let navigation_keys tx =
  let tags = bindtags_get tx in
    match tags with
      (WidgetBindings t)::l when t = tx ->
      	bindtags tx ((WidgetBindings tx) :: (TagBindings "TEXT_RO") :: l)
    | _ -> ()
;;
let new_scrollable_text top options navigation =
  let f = frame__create top [] in
  let tx = text__create f options 
  and sb = scrollbar__create f [] in
    scroll_link sb tx;
    (* IN THIS ORDER -- RESIZING *)
    pack [sb] [Side Side_Right; Fill Fill_Y];
    pack [tx] [Side Side_Left; Fill Fill_Both; Expand true];
    if navigation then navigation_keys tx;
    f, tx
;;
(*
 * Searching
 *)
let patternv = frx_misc__autodef textvariable__new
;;
let topsearch t =
  (* The user interface *)
  let top = toplevelw__create t [Class "TextSearch"] in
  wm__title_set top "Text search";
    let f = frame__create_named top "fpattern" [] in
      let m = label__create_named f "search" [Text "Search pattern"]
      and e = entry__create_named f "pattern" 
      	[Relief Sunken; TextVariable (patternv()) ] in
  let hgroup = frame__create top []
  and bgroup = frame__create top [] in
    let fdir = frame__create hgroup [] 
    and fmisc = frame__create hgroup [] in
    let direction = textvariable__new_temporary fdir
    and exactv = textvariable__new_temporary fdir
    and casev = textvariable__new_temporary fdir in
       let forw = radiobutton__create_named fdir "forward"
	     [Text "Forward"; Variable direction; Value "f"]
      and backw = radiobutton__create_named fdir "backward"
	     [Text "Backward"; Variable direction; Value "b"]
      and exact = checkbutton__create_named fmisc "exact"
	     [Text "Exact match"; Variable exactv]
      and case = checkbutton__create_named fmisc "case"
	     [Text "Fold Case"; Variable casev] 
      and searchb = button__create_named bgroup "search" [Text "Search"]
      and contb = button__create_named bgroup "continue" [Text "Continue"]
      and dismissb = button__create_named bgroup "dismiss"
	 [Text "Dismiss"; 
         Command (fun () -> text__tag_delete t ["search"]; destroy top)] in

      radiobutton__invoke forw;
      pack [m][Side Side_Left];
      pack [e][Side Side_Right; Fill Fill_X; Expand true];
      pack [forw; backw] [Anchor W];
      pack [exact; case] [Anchor W];
      pack [fdir; fmisc] [Side Side_Left; Anchor Center];
      pack [searchb; contb; dismissb] [Side Side_Left; Fill Fill_X];
      pack [f;hgroup;bgroup] [Fill Fill_X; Expand true];

  let current_index = ref textBegin in

   let search cont = fun () ->
     let opts = ref [] in
     if textvariable__get direction = "f" then
      	opts := Forwards :: !opts
     else opts := Backwards :: !opts ;
     if textvariable__get exactv = "1" then
       opts := Exact :: !opts;
     if textvariable__get casev = "1" then
       opts := Nocase :: !opts;
     try
       let forward = textvariable__get direction = "f" in
       let i = text__search t !opts (entry__get e)
      	  (if cont then !current_index 
      	   else if forward then textBegin
	   else TextIndex(End, [CharOffset (-1)])) (* does not work with end *)
	  (if forward then textEnd 
      	   else textBegin) in
       let found = TextIndex (i, []) in
       	 current_index := 
      	   TextIndex(i, [CharOffset (if forward then 1 else (-1))]);
	 text__tag_delete t ["search"];
	 text__tag_add t "search" found (TextIndex (i, [WordEnd]));
	 text__tag_configure t "search" 
      	       	[Relief Raised; BorderWidth (Pixels 1);
      	       	 Background Red];
	 text__see t found
     with
       Invalid_argument _ -> bell__ring() in
    
   bind e [[], KeyPressDetail "Return"] 
      	 (BindSet ([], fun _ -> search false ()));
   button__configure searchb [Command (search false)];
   button__configure contb [Command (search true)];
   tkwait__visibility top;
   focus__set e
;;
let addsearch tx =
  let tags = bindtags_get tx in
    match tags with
      (WidgetBindings t)::l when t = tx ->
      	bindtags tx ((WidgetBindings tx) :: (TagBindings "SEARCH") :: l)
    | _ -> ()
;;
(* We use Mod1 instead of Meta or Alt *)
let init () = 
  do_list (function ev ->
      	     tag_bind "TEXT_RO" ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> page_up ei.ev_Widget; break()))))
	   [
	    [[], KeyPressDetail "BackSpace"];
	    [[], KeyPressDetail "Delete"];
	    [[], KeyPressDetail "Prior"];
	    [[], KeyPressDetail "b"];
	    [[Mod1], KeyPressDetail "v"]
	   ];
  do_list (function ev ->
      	     tag_bind "TEXT_RO" ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> page_down ei.ev_Widget; break()))))
	   [
	    [[], KeyPressDetail "space"];
	    [[], KeyPressDetail "Next"];
	    [[Control], KeyPressDetail "v"]
	   ];
  do_list (function ev ->
      	     tag_bind "TEXT_RO" ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> line_up ei.ev_Widget; break()))))
	   [
	    [[], KeyPressDetail "Up"];
	    [[Mod1], KeyPressDetail "z"]
	   ];
  do_list (function ev ->
      	     tag_bind "TEXT_RO" ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> line_down ei.ev_Widget; break()))))
	   [
	    [[], KeyPressDetail "Down"];
	    [[Control], KeyPressDetail "z"]
	   ];

  do_list (function ev ->
      	     tag_bind "TEXT_RO" ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> top ei.ev_Widget; break()))))
	   [
	    [[], KeyPressDetail "Home"];
	    [[Mod1], KeyPressDetail "less"]
	   ];

  do_list (function ev ->
      	     tag_bind "TEXT_RO" ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> bottom ei.ev_Widget; break()))))
	   [
	    [[], KeyPressDetail "End"];
	    [[Mod1], KeyPressDetail "greater"]
	   ];

  do_list (function ev ->
      	      tag_bind "SEARCH" ev
	           (BindSetBreakable ([Ev_Widget],
		             (fun ei -> topsearch ei.ev_Widget; break()))))
	   [
            [[Control], KeyPressDetail "s"]
	   ]

;;
