(* A trick by Steve Ball to do pixel scrolling on text widgets *)
(* USES frx_fit *)
#open "tk";;

let create top opts navigation =
  let f = frame__create top [BorderWidth (Pixels 2); Relief Raised] in
  let lf = frame__create f [] in
  let rf = frame__create f [] in
  let c = canvas__create lf [BorderWidth (Pixels 0)]
  and xscroll = scrollbar__create lf [Orient Horizontal]
  and yscroll = scrollbar__create rf [Orient Vertical] 
  and secret = frame__create_named rf "secret" []
  in
  let t = text__create c (BorderWidth(Pixels 0) :: opts) in
    if navigation then frx_text__navigation_keys t;
 
    (* Make the text widget an embedded canvas object *)
    let _ =
     canvas__create_window c (Pixels 0) (Pixels 0)
                           [Anchor NW; Window t; Tags [Tag "main"]] in
    canvas__focus c (Tag "main");
    (*
    canvas__configure c [Width (Pixels (winfo__reqwidth t));
      	       	        Height(Pixels (winfo__reqheight t))];
    *)
    canvas__configure c [YScrollCommand (scrollbar__set yscroll)];
    (* The horizontal scrollbar is directly attached to the
     * text widget, because h scrolling works properly *)
    scrollbar__configure xscroll [ScrollCommand (text__xview t)];
    (* But vertical scroll is attached to the canvas *)
    scrollbar__configure yscroll [ScrollCommand (canvas__yview c)];
    let scroll, check = frx_fit__vert t in
    text__configure t [
      	XScrollCommand (scrollbar__set xscroll);
        YScrollCommand (fun first last ->
      	   scroll first last;
	   let x,y,w,h = canvas__bbox c [Tag "main"] in
	     canvas__configure c 
      	       [ScrollRegion (Pixels x, Pixels y, Pixels w, Pixels h)])
        ];

    bind c [[],Configure] (BindSet ([Ev_Width], (fun ei ->
      canvas__configure_window c (Tag "main") [Width (Pixels ei.ev_Width)])));

    pack [rf] [Side Side_Right; Fill Fill_Y];
    pack [lf] [Side Side_Left; Fill Fill_Both; Expand true];
    pack [secret] [Side Side_Bottom];
    pack [yscroll] [Side Side_Top; Fill Fill_Y; Expand true];
    pack [xscroll] [Side Side_Bottom; Fill Fill_X];
    pack [c] [Side Side_Left; Fill Fill_Both; Expand true];
    f, t
;;
