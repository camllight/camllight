#open "tk";;

let version = "$Id$"
;;

(* 
 * convert an integer to an absolute index 
*)
let abs_index n =
  TextIndex (LineChar(0,0), [CharOffset n])
;;

let InsertMark =
  TextIndex(Mark "insert", [])
;;

let CurrentMark =
  TextIndex(Mark "current", [])
;;

let TextEnd =
  TextIndex(End, [])
;;

let TextBegin =
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
and top tx = text__yview_index tx TextBegin
and bottom tx = text__yview_index tx TextEnd
;;

(* We use Mod1 instead of Meta or Alt *)

let init () = 
  do_list (function ev ->
      	     tag_bind "TEXT_RO" ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> page_up ei.Ev_Widget; break()))))
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
      	       	       	       	 (fun ei -> page_down ei.Ev_Widget; break()))))
	   [
	    [[], KeyPressDetail "space"];
	    [[], KeyPressDetail "Next"];
	    [[Control], KeyPressDetail "v"]
	   ];
  do_list (function ev ->
      	     tag_bind "TEXT_RO" ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> line_up ei.Ev_Widget; break()))))
	   [
	    [[], KeyPressDetail "Up"];
	    [[Mod1], KeyPressDetail "z"]
	   ];
  do_list (function ev ->
      	     tag_bind "TEXT_RO" ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> line_down ei.Ev_Widget; break()))))
	   [
	    [[], KeyPressDetail "Down"];
	    [[Control], KeyPressDetail "z"]
	   ];

  do_list (function ev ->
      	     tag_bind "TEXT_RO" ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> top ei.Ev_Widget; break()))))
	   [
	    [[], KeyPressDetail "Home"];
	    [[Mod1], KeyPressDetail "less"];
	   ];

  do_list (function ev ->
      	     tag_bind "TEXT_RO" ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> bottom ei.Ev_Widget; break()))))
	   [
	    [[], KeyPressDetail "End"];
	    [[Mod1], KeyPressDetail "greater"];
	   ]

;;

let navigation_keys tx =
   bindtags tx [WidgetBindings tx; TagBindings "TEXT_RO"; 
      	        TagBindings "Text"; TagBindings "."; TagBindings "all"]
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

let topsearch t =
  (* The user interface *)
  let top = toplevelw__create t [] in
  wm__title_set top "Text search";
    let f = frame__create top [] in
      let m = label__create f [Text "Search pattern"]
      and e = entry__create f [Relief Sunken] in
  let hgroup = frame__create top []
  and bgroup = frame__create top [] in
    let fdir = frame__create hgroup [] 
    and fmisc = frame__create hgroup [] 
    and direction = textvariable__new ()
    and exactv = textvariable__new ()
    and casev = textvariable__new () in
       let forw = radiobutton__create fdir 
	     [Text "Forward"; Variable direction; Value "f"]
      and backw = radiobutton__create fdir
	     [Text "Backward"; Variable direction; Value "b"]
      and exact = checkbutton__create fmisc
	     [Text "Exact match"; Variable exactv]
      and case = checkbutton__create fmisc
	     [Text "Fold Case"; Variable casev] 
      and searchb = button__create bgroup [Text "Search"]
      and contb = button__create bgroup [Text "Continue"]
      and dismissb = button__create bgroup 
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

  let current_index = ref TextBegin in

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
      	   else if forward then TextBegin
	   else TextIndex(End, [CharOffset (-1)])) (* does not work with end *)
	  (if forward then TextEnd 
      	   else TextBegin) in
       let found = TextIndex (i, []) in
       	 current_index := 
      	   TextIndex(i, [CharOffset (if forward then 1 else (-1))]);
	 text__tag_delete t ["search"];
	 text__tag_add t "search" found (TextIndex (i, [WordEnd]));
	 text__tag_configure t "search" 
      	       	[Relief Raised; BorderWidth (Pixels 1);
      	       	 Background Red];
	 text__see t found;
     with
       Invalid_argument _ -> bell__ring() in
    
   bind e [[], KeyPressDetail "Return"] 
      	 (BindSet ([], fun _ -> search false ()));
   button__configure searchb [Command (search false)];
   button__configure contb [Command (search true)];
   focus__set e
;;

let addsearch t =
  bind t
    [[Control], KeyPressDetail "s"]
    (BindSet ([], (fun _ -> topsearch t)))
;;
