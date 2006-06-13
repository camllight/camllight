#open "format";;
#open "communication";;
#open "checkpoints";;
#open "events";;
#open "frames";;
#open "show_source";;
#open "breakpoints";;
#open "builtins";;
#open "pr_value";;

(* Display information about the current event. *)
let show_current_event () =
  print_string "Time : "; print_int (current_time ());
  (match current_pc () with
     Some pc ->
       print_string " - pc : "; print_int pc
   | _ -> ());
  update_current_event ();
  reset_frame ();
  match current_report ()  with
    None ->
      print_newline ();
      print_string "Beginning of program."; print_newline ();
      show_no_point ()
  | Some {Rep_type = (Rep_event | Rep_breakpoint); Rep_program_pointer = pc} ->
      let (module, point) = current_point () in
	print_string (" - module " ^ module);
      	print_newline ();
	(match breakpoints_at_pc pc with
	   [] ->
      	     ()
	 | [breakpoint] ->
	     print_string "Breakpoint : "; print_int breakpoint;
             print_newline ()
	 | breakpoints ->
	     print_string "Breakpoints : ";
	     do_list (function x -> print_int x; print_string " ") breakpoints;
      	     print_newline ());
        show_point module point (current_event_is_before ()) true
  | Some {Rep_type = Rep_exited} ->
      print_newline (); print_string "Program end."; print_newline ();
      show_no_point ()
  | Some {Rep_type = Rep_exc} ->
      print_newline ();
      print_string "Program end.";
      print_newline ();
      open_box 0;
      print_string "Uncaught exception:"; print_space();
      print_value (get_accu ()) type_exn;
      close_box();
      print_newline();
      show_no_point ()
  | Some {Rep_type = Rep_trap} ->
					(* Rep_trap not visible outside *)
	       	       	       	       	(* of module `time_travel'. *)
      failwith "???show_current_event";;

(* Display information about the current frame. *)
(* --- `select frame' must have succeded before calling this function. *)
let show_current_frame selected =
  print_string "#";
  print_int !current_frame;
  print_string "  Pc : ";
  print_int !selected_program_counter;
  try
    let (module, point) = selected_point () in
      print_string (" - module " ^ module);
      print_newline ();
      (match breakpoints_at_pc !selected_program_counter with
	 [] ->
      	   ()
       | [breakpoint] ->
	   print_string "Breakpoint : "; print_int breakpoint; print_newline ()
       | breakpoints ->
	   print_string "Breakpoints : ";
	   do_list (function x -> print_int x; print_string " ") breakpoints;
      	   print_newline ());
      show_point module point (selected_event_is_before ()) selected
  with
    Not_found ->
      print_newline ();
      print_string "No event there.";
      print_newline ();;
