#open "tk";;

let version = "$Id$"
;;

(* 
 * Link a scrollbar and a text widget 
*)
let scroll_link sb tx =
  text__configure tx [YScrollCommand (scrollbar__set sb)];
  scrollbar__configure sb [ScrollCommand (text__yview_scroll tx)]
;;


(*
 * Tk 4.0 has navigation in Text widgets, sometimes using scrolling
 * sometimes using the insertion mark. It is a pain to add more
 * compatible bindings. We do our own.
 *)
let page_up tx   =  text__yview_scroll tx (ScrollPage (-1))
and page_down tx =  text__yview_scroll tx (ScrollPage 1)
and line_up tx   =  text__yview_scroll tx (ScrollUnit (-1))
and line_down tx =  text__yview_scroll tx (ScrollUnit 1)
;;

let init () = 
  do_list (function ev ->
      	     tag_bind "TEXT_RO" ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> page_up ei.Ev_Widget; break()))))
	   [
	    [[], XKey "BackSpace"];
	    [[], XKey "Delete"];
	    [[], XKey "Prior"];
	    [[], XKey "b"];
	    [[Meta], XKey "v"]
	   ];
  do_list (function ev ->
      	     tag_bind "TEXT_RO" ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> page_down ei.Ev_Widget; break()))))
	   [
	    [[], XKey "space"];
	    [[], XKey "Next"];
	    [[Control], XKey "v"]
	   ];
  do_list (function ev ->
      	     tag_bind "TEXT_RO" ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> line_up ei.Ev_Widget; break()))))
	   [
	    [[], XKey "Up"];
	    [[Meta], XKey "z"]
	   ];
  do_list (function ev ->
      	     tag_bind "TEXT_RO" ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> line_down ei.Ev_Widget; break()))))
	   [
	    [[], XKey "Down"];
	    [[Control], XKey "z"]
	   ]
;;

let navigation_keys tx =
   bindtags tx [WidgetBindings tx; TagBindings "TEXT_RO"; 
      	        TagBindings "Text"; TagBindings "All"]
;;

let new_scrollable_text top options navigation =
  let f = frame__create top [] in
  let tx = text__create f options 
  and sb = scrollbar__create f [] in
    scroll_link sb tx;
    pack [tx] [Side Side_Left; Fill Fill_Both; Expand true];
    pack [sb] [Side Side_Left; Fill Fill_Y];
    if navigation then navigation_keys tx;
    f, tx
;;

(* 
 * convert an integer to an absolute index 
*)
let abs_index n =
  TextIndex (TI_LineChar(0,0), [CharOffset n])
;;

let InsertMark =
  TextIndex(TI_Mark "insert", [])
;;

let CurrentMark =
  TextIndex(TI_Mark "current", [])
;;
