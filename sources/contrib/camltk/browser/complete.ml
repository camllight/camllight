(* Completion for listboxes *) 
(*
   - assumes sorted list
   - assumes fixed size
   - single select
*)
#open "tk";;

let add_completion lb action feedback =
  listboxSingleSelect [lb];
  let prefx = ref ""		  (* current match prefix *)
  and size = listbox__size lb     (* to check bounds *)
  and current = ref 0 in

  let set_current () = 
    current := match listbox__curselection lb with
       [] ->  listbox__select_from lb (Number 0); 0
     | [Number x] -> x in

  let rec move_forward () =
    if compare_strings (listbox__get lb (Number !current)) !prefx < 0 then
      if !current < size then begin incr current; move_forward() end

  and  move_backward () =
    if compare_strings (listbox__get lb (Number !current)) !prefx > 0 then
      if !current > 0 then begin decr current; move_backward() end

  and recenter () =
     listbox__select_from lb (Number !current);
     listbox__yview lb (Number (max 0 (!current - 3))) in

  let backspace _ =
      if !prefx <> "" then begin
      	set_current();
      	prefx := sub_string !prefx 0 (pred (string_length !prefx));
        feedback !prefx;
	move_backward();
	recenter()
        end
  and forward s =
      set_current();
      prefx := !prefx ^ s;
      feedback !prefx;
      move_forward();
      recenter() in

  (* backspace keys *)
  do_list (function ev ->
      	    bind lb ev (BindSet([], backspace)))
   [
    [[], XKey "BackSpace"];
    [[], XKey "Delete"];
    [[Control], XKey "h"];
   ];
  bind lb [[Any], KeyPress] 
      (BindSet([Ev_Char], (function ev -> forward ev.Ev_Char)));
  (* validate *)
  bind lb [[], XKey "Return"] (BindSet([], action));
  (* Finally, we have to set focus, otherwise events dont get through *)
  bind lb [[Any],Enter] (BindSet ([],fun _ -> focus__set lb));
  bind lb [[Any],Leave] (BindSet ([],fun _ -> focus__none ()));
  (* Returns the function that needs to update completion internal state *)
  (function _ ->
    set_current();
    prefx := listbox__get lb (Number !current);
    feedback !prefx)
;;
