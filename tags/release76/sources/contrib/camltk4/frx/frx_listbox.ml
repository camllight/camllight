#open "tk";;

let version = "$Id$"
;;
(* 
 * Link a scrollbar and a listbox
 *)
let scroll_link sb lb =
  listbox__configure lb 
      	[YScrollCommand (scrollbar__set sb)];
  scrollbar__configure sb 
        [ScrollCommand (listbox__yview lb)]
;;
(* 
 * Completion for listboxes, Macintosh style. 
 * As long as you type fast enough, the listbox is repositioned to the
 * first entry "greater" than the typed prefix.
 * assumes: 
 *   sorted list (otherwise it's stupid)
 *   fixed size, because we don't recompute size at each callback invocation
 *)

let add_completion lb action =
  let prefx = ref ""		  (* current match prefix *)
  and maxi = listbox__size lb - 1 (* maximum index (doesn't matter actually) *)
  and current = ref 0 		  (* current position *)
  and lastevent = ref 0 in

  let rec move_forward () =
    if listbox__get lb (Number !current) < !prefx then
      if !current < maxi then begin incr current; move_forward() end

  and recenter () =
    let element = Number !current in
     (* Clean the selection *)
     listbox__selection_clear lb (Number 0) End;
     (* Set it to our unique element *)
     listbox__selection_set lb element element;
     (* Activate it, to keep consistent with Up/down.
        You have to be in Extended or Browse mode *)
     listbox__activate lb element;
     listbox__selection_anchor lb element;
     listbox__see lb element in

  let complete time s =
    if time - !lastevent < 500 then   (* sorry, hard coded limit *)
      prefx := !prefx ^ s
    else begin (* reset *)
      current := 0;
      prefx := s
    end;
    lastevent := time;
    move_forward();
    recenter() in


  bind lb [[], KeyPress] 
      (BindSet([Ev_Char; Ev_Time], 
      	  (function ev -> 
	     (* consider only keys producing characters. The callback is called
	      * even if you press Shift.
              *)
      	     if ev.ev_Char <> "" then complete ev.ev_Time ev.ev_Char)));
  (* Key specific bindings override KeyPress *)
  bind lb [[], KeyPressDetail "Return"] (BindSet([], action));
  (* Finally, we have to set focus, otherwise events dont get through *)
  focus__set lb;
  recenter()   (* so that first item is selected *)
;;

let new_scrollable_listbox top options =
  let f = frame__create top [] in
  let lb = listbox__create f options 
  and sb = scrollbar__create f [] in
    scroll_link sb lb;
    pack [lb] [Side Side_Left; Fill Fill_Both; Expand true];
    pack [sb] [Side Side_Left; Fill Fill_Y];
    f, lb
;;
