(* Completion for listboxes *) 
(*
   - assumes sorted list
   - assumes fixed size
   - single select
*)
#open "tk";;

let add_completion lb action =
  let prefx = ref ""		  (* current match prefix *)
  and size = listbox__size lb     (* to check bounds *)
  and current = ref 0 		  (* current position *)
  and lastevent = ref 0 in

  let rec move_forward () =
    if compare_strings (listbox__get lb (Number !current)) !prefx < 0 then
      if !current < size then begin incr current; move_forward() end

  and recenter () =
     listbox__select_from lb (Number !current);
     listbox__yview lb (Number (max 0 (!current - 3))) in

  (* sorry, hard coded limit *)
  let complete time s =
    if time - !lastevent < 500 then
      prefx := !prefx ^ s
    else begin (* reset *)
      current := 0;
      prefx := s;
    end;
    lastevent := time;
    move_forward();
    recenter() in


  bind lb [[Any], KeyPress] 
      (BindSet([Ev_Char; Ev_Time], 
      	  (function ev -> if ev.Ev_Char <> "" then
      	                    complete ev.Ev_Time ev.Ev_Char)));
  (* validate *)
  bind lb [[], XKey "Return"] (BindSet([], action));
  (* Finally, we have to set focus, otherwise events dont get through *)
  bind lb [[Any],Enter] (BindSet ([],fun _ -> focus__set lb));
  bind lb [[Any],Leave] (BindSet ([],fun _ -> focus__none ()))
;;
