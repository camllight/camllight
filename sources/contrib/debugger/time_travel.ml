(**************************** Time travel ***********************)

#open "debugger_config";;
#open "misc";;
#open "primitives";;
#open "interruption";;
#open "communication";;
#open "input_handling";;
#open "events";;
#open "checkpoints";;
#open "breakpoints";;
#open "trap_barrier";;
#open "program_loading";;

(*** Debugging. ***)

let debug_time_travel = ref false;;

(*** Internal utilities. ***)

(* Select a checkpoint at current. *)
let set_current_checkpoint checkpoint =
  current_checkpoint := checkpoint;
  set_current_connection checkpoint.C_fd;;

(* Insert a checkpoint in the checkpoint list. *)
(* Raise `Exit' if there is already a checkpoint at the same time. *)
let insert_checkpoint ({C_time = time} as checkpoint) =
  let rec traverse =
    function
      [] -> [checkpoint]
    | (({C_time = t} as a)::l) as l' ->
        if t > time then
	  a::(traverse l)
	else if t = time then
	  raise Exit
	else
          checkpoint::l'
  in
    checkpoints := traverse !checkpoints;;

(* Remove a checkpoint from the checkpoint list *)
(* --- No error if not found. *)
let remove_checkpoint checkpoint =
  let rec traverse =
    function
      [] -> []
    | a::l ->
      	if a == checkpoint then
	  l
	else
	  a::(traverse l)
  in
    checkpoints := traverse !checkpoints;;

(* Wait for the processus used by `checkpoint' to connect. *)
(* --- Usually not called (the process is already connected). *)
let wait_for_connection checkpoint =
  print_string "Waiting for connection with process ";
  print_int checkpoint.C_pid;
  print_string "...";
  print_newline ();
  try
    exec_unprotected
      (function () ->
         let old_controller = current_controller !connection in
           execute_with_other_controller
             (function
      	        fd ->
                  old_controller fd;
                  if checkpoint.C_valid = true then
	            exit_main_loop ())
             !connection
             main_loop)
  with
    sys__Break ->
      checkpoint.C_parent <- root;
      remove_checkpoint checkpoint;
      checkpoint.C_pid <- -1;
      raise sys__Break;;

(* Kill `checkpoint'. *)
let kill_checkpoint checkpoint =
  if !debug_time_travel then
    prerr_endline ("Kill : " ^ (string_of_int checkpoint.C_pid));
  if checkpoint.C_pid > 0 then		(* Ghosts don't have to be killed ! *)
    (if not checkpoint.C_valid then
       wait_for_connection checkpoint;
     stop checkpoint.C_fd;
     if checkpoint.C_parent.C_pid > 0 then
       wait_child checkpoint.C_parent.C_fd;
     checkpoint.C_parent <- root;
     close_io checkpoint.C_fd;
     remove_file checkpoint.C_fd;
     remove_checkpoint checkpoint);
  checkpoint.C_pid <- -1;;		(* Don't exist anymore *)

(*** Cleaning the checkpoint list. ***)

(* Separe checkpoints before (<=) and after (>) `t'. *)
(* ### t checkpoints -> (after, before) *)
let cut t =
  let rec cut_t =
    function
      [] -> ([], [])
    | ({C_time = t'} as a::l) as l' ->
        if t' <= t then
      	  ([], l')
        else
      	  let (b, e) = cut_t l in
      	    (a::b, e)
  in
    cut_t;;

(* Partition the checkpoints list. *)
let cut2 t0 t l =
  let rec cut2_t0 t =
    function
      [] -> []
    | l ->
       let (after, before) = cut (t0 - t - 1) l in
         let l = cut2_t0 (2 * t) before in
       	   after::l
  in
    let (after, before) = cut (t0 - 1) l in
      after::(cut2_t0 t before);;

(* Separe first elements and last element of a list of checkpoint. *)
let chk_merge2 cont =
  let rec chk_merge2_cont =
    function
      [] -> cont
    | [a] ->
        let (accepted, rejected) = cont in
          (a::accepted, rejected)
    | a::l ->
        let (accepted, rejected) = chk_merge2_cont l in
      	  (accepted, a::rejected)
  in chk_merge2_cont;;

(* Separe the checkpoint list. *)
(* ### list -> accepted * rejected *)
let rec chk_merge =
  function
    [] -> ([], [])
  | l::tail ->
       chk_merge2 (chk_merge tail) l;;

let new_checkpoint_list checkpoint_count accepted rejected =
  if list_length accepted >= checkpoint_count then
    let (k, l) = list_truncate2 checkpoint_count accepted in
      (k, l @ rejected)
  else
    let (k, l) =
      list_truncate2 (checkpoint_count - list_length accepted) rejected
    in
      (sort__merge (fun {C_time = t1} {C_time = t2} -> t1 > t2) accepted k,
       l);;

(* Clean the checkpoint list. *)
(* Reference time is `time'. *)
let clean_checkpoints time checkpoint_count =
  let (after, before) = cut time !checkpoints in
    let (accepted, rejected) =
      chk_merge (cut2 time !checkpoint_small_step before)
    in
      let (kept, lost) =
        new_checkpoint_list checkpoint_count accepted after
      in
	do_list kill_checkpoint (lost @ rejected);
      	checkpoints := kept;;

(*** Internal functions for moving. ***)

(* Find the first checkpoint before (or at) `time'. *)
(* Ask for reloading the program if necessary. *)
(* ### time -> checkpoint *)
let find_checkpoint_before time =
  let rec find =
    function
      [] ->
      	print_string "Can't go that far in the past !"; print_newline ();
      	if yes_or_no "Reload program" then
      	  (load_program ();
      	   find !checkpoints)
	else
	  raise Toplevel
    | {C_time = t} as a::l ->
      	if t > time then
          find l
	else
	  a
  in find !checkpoints;;

(* Make a copy of the current checkpoint and clean the checkpoint list. *)
(* --- The new checkpoint in not put in the list. *)
let duplicate_current_checkpoint () =
  let checkpoint = !current_checkpoint in
    if not checkpoint.C_valid then
      wait_for_connection checkpoint;
    let new_checkpoint =			(* Ghost *)
      {C_time = checkpoint.C_time;
       C_pid = 0;
       C_fd = checkpoint.C_fd;
       C_valid = false;
       C_report = checkpoint.C_report;
       C_state = C_stopped;
       C_parent = checkpoint;
       C_breakpoint_version = checkpoint.C_breakpoint_version;
       C_breakpoints = checkpoint.C_breakpoints;
       C_trap_barrier = checkpoint.C_trap_barrier}
    in
      checkpoints := list_replace checkpoint new_checkpoint !checkpoints;
      set_current_checkpoint checkpoint;
      clean_checkpoints (checkpoint.C_time + 1) (!checkpoint_max_count - 1);
      if new_checkpoint.C_pid = 0 then	(* The ghost has not been killed *)
        (match do_checkpoint () with	(* Duplicate checkpoint *)
           Checkpoint_done pid ->
	     (new_checkpoint.C_pid <- pid;
	      if !debug_time_travel then
                prerr_endline ("Attente de connection : " ^ (string_of_int pid)))
         | Checkpoint_failed ->
      	     prerr_endline
               "A fork failed. The maximum number of checkpoints is reduced.";
      	     checkpoint_max_count := list_length !checkpoints - 1;
       	     remove_checkpoint new_checkpoint);;

(* Select either `go' or `restart' and execute it. *)
let select_go duration =
  match
    (match current_report () with	(* Are we on a breakpoint ? *)
       None ->
  	 None
     | Some {Rep_program_pointer = pc} ->
         instruction_at_pc pc)
  with
    None ->				(* Go. *)
      go duration
  | Some instr ->
      restart instr duration;;

(* Ensure we stop on an event. *)
let rec stop_on_event report =
  let find_event () =
    if !debug_time_travel then
      (print_string "Searching next event..."; print_newline ());
    let report = select_go 1 in
      !current_checkpoint.C_report <- Some report;
      stop_on_event report
  in
    match report with
      {Rep_type = Rep_breakpoint; Rep_program_pointer = pc} ->
      	if Some pc = !temporary_breakpoint_position then
					(* Others breakpoints are on events. *)
          (try				(* Check if we are on an event. *)
      	     update_current_event ()
           with
       	     Not_found ->
	       find_event ())
    | {Rep_type = Rep_trap; Rep_stack_pointer = trap_frame} ->
					(* No event at current position. *)
	  find_event ()
    | _ ->
      	();;

(* Was the movement interrupted ? *)
(* --- An exception could have been used instead, *)
(* --- but it is not clear where it should be caught. *)
(* --- For instance, we can't caught it should not be caught in `step' *)
(* --- (as `step' is used in `next_1'). *)
(* --- On the other side, other modules does not need to know *)
(* --- about this exception. *)
let interrupted = ref false;;

(* Internal function for running debugged program. *)
(* --- Require `duration > 0' *)
let internal_step duration =
  match current_report () with
    (Some ({Rep_type = Rep_exited | Rep_exc} as x)) ->
      ()				(* End of program. *)
  | _ ->
      exec_protected
        (function () ->
	   if !make_checkpoints then
             duplicate_current_checkpoint ()
	   else
	     remove_checkpoint !current_checkpoint;
           update_breakpoints ();
           update_trap_barrier ();
	   !current_checkpoint.C_state <- C_running duration;
           let report = select_go duration in
             !current_checkpoint.C_report <- Some report;
	     !current_checkpoint.C_state <- C_stopped;
	     if report.Rep_type = Rep_event then
               (!current_checkpoint.C_time <-
                  !current_checkpoint.C_time + duration;
	        interrupted := false)
	     else
               (!current_checkpoint.C_time <-
      	           !current_checkpoint.C_time + duration
      	       	       - report.Rep_event_count + 1;
	        interrupted := true;
	        stop_on_event report);
             (try
                insert_checkpoint !current_checkpoint
              with
                Exit ->
                  kill_checkpoint !current_checkpoint;
                  set_current_checkpoint
      	            (find_checkpoint_before (current_time ()))));
	if !debug_time_travel then
          (print_string "Checkpoints : pid(time)"; print_newline ();
           do_list
             (function {C_time = time; C_pid = pid; C_valid = valid} ->
      	        print_int pid; print_string "("; print_int time;
      	        print_string ")"; if not valid then print_string "(invalid)";
      	        print_string " ")
      	     !checkpoints;
      	   print_newline ());;

(*** Miscellaneous functions (exported). ***)

(* Create a checkpoint at time 0 (new program). *)
let new_checkpoint pid fd =
  let new_checkpoint =
    {C_time = 0;
     C_pid = pid;
     C_fd = fd;
     C_valid = true;
     C_report = None;
     C_state = C_stopped;
     C_parent = root;
     C_breakpoint_version = 0;
     C_breakpoints = [];
     C_trap_barrier = 0}
  in
    insert_checkpoint new_checkpoint;;

(* Set the file descriptor of a checkpoint *)
(* (a new process has connected with the debugger). *)
(* --- Return `true' on success (close the connection otherwise). *)
let set_file_descriptor pid fd =
  let rec find =
    function
      [] ->
	prerr_endline "Unexpected connection";
	close_io fd;
      	false
    | ({C_pid = pid'} as checkpoint)::l ->
        if pid <> pid' then
	  find l
	else
	  (checkpoint.C_fd <- fd;
	   checkpoint.C_valid <- true;
	   true)
  in
    if !debug_time_travel then
      prerr_endline ("Nouvelle connection : " ^(string_of_int pid));
    find (!current_checkpoint::!checkpoints);;

(* Kill all the checkpoints. *)
let kill_all_checkpoints () =
  do_list kill_checkpoint (!current_checkpoint::!checkpoints);;

(* Kill a checkpoint without killing the process. *)
(* (used when connection with the process is lost). *)
(* --- Assume that the checkpoint is valid. *)
let forget_process fd pid =
  let checkpoint =
    find (function c -> c.C_pid = pid) (!current_checkpoint::!checkpoints)
  in
    prerr_string "Lost connection with process ";
    prerr_int pid;
    if checkpoint = !current_checkpoint then
      (prerr_endline " (active process)";
       match !current_checkpoint.C_state with
       	 C_stopped ->
	   prerr_string "at time ";
	   prerr_int !current_checkpoint.C_time
       | C_running duration ->
           prerr_string "between time ";
           prerr_int !current_checkpoint.C_time;
           prerr_string " and time ";
           prerr_int (!current_checkpoint.C_time + duration));
    prerr_endline "";
    input_handling__remove_file fd;
    close_io checkpoint.C_fd;
    remove_file checkpoint.C_fd;
    remove_checkpoint checkpoint;
    checkpoint.C_pid <- -1;		(* Don't exist anymore *)
    if checkpoint.C_parent.C_pid > 0 then
      wait_child checkpoint.C_parent.C_fd;
    if checkpoint = !current_checkpoint then
      raise Current_checkpoint_lost;;

(* Try to recover when the current checkpoint is lost. *)
let recover () =
  set_current_checkpoint
    (find_checkpoint_before (current_time ()));;

(*** Simple movements. ***)

(* Forward stepping. *)
(* --- Require `duration >= 0'. *)
let rec step_forward duration =
  if duration > !checkpoint_small_step then
    let first_step =
      if duration > !checkpoint_big_step then
      	!checkpoint_big_step
      else
      	!checkpoint_small_step
    in
      (internal_step first_step;
       if not !interrupted then
         step_forward (duration - first_step))
  else if duration != 0 then
    internal_step duration;;

(* Go to time `time' from current checkpoint (internal). *)
let internal_go_to time =
  let duration = time - current_time () in
    if duration > 0 then
      execute_without_breakpoints (function () -> step_forward duration);;

(* Move to a given time. *)
let go_to time =
  let {C_time = t} as checkpoint = find_checkpoint_before time in
    set_current_checkpoint checkpoint;
    internal_go_to time;;

(* Return the time of the last breakpoint *)
(* between current time and `max_time'. *)
let rec find_last_breakpoint max_time =
  let on_breakpoint () =
    match current_report () with
      Some {Rep_program_pointer = pc} ->
      	breakpoint_at_pc pc
    | _ ->
      	false
  in
    let rec find break =
      let time = current_time () in
        step_forward (max_time - time);
	if ((on_breakpoint ()) && (current_time () < max_time)) then
	  find true
	else
	  (time, break)
    in
      find (on_breakpoint ());;

(* Run from `time_max' back to `time'. *)
(* --- Assume 0 <= time < time_max *)
let rec back_to time time_max =
  let
    {C_time = t} as checkpoint = find_checkpoint_before (time_max - 1)
  in
    go_to (max time t);
    let (new_time, break) = find_last_breakpoint time_max in
      if break || (new_time <= time) then
        go_to new_time
      else
      	back_to time new_time;;

(* Backward stepping. *)
(* --- Assume duration > 1 *)
let step_backward duration =
  let time = current_time () in
    if time > 0 then
      back_to (max 0 (time - duration)) time;;

(* Run the program from current time. *)
(* Stop at the first breakpoint, or at the end of the program. *)
let rec run () =
  internal_step !checkpoint_big_step;
  if not !interrupted then
    run ();;

(* Run backward the program form current time. *)
(* Stop at the first breakpoint, or at the beginning of the program. *)
let back_run () =
  if current_time () > 0 then
    back_to 0 (current_time ());;

(* Step in any direction. *)
(* Stop at the first brakpoint, or after `duration' steps. *)
let step duration =
  if duration >= 0 then
    step_forward duration
  else
    step_backward (-duration);;

(*** Next, finish. ***)

(* Finish current fucntion. *)
let finish () =
  match move_frame 1 with
    None ->
      prerr_endline "`finish' not meaningful in the outermost frame.";
      raise Toplevel
  | Some (frame, pc) ->
      exec_with_trap_barrier
      	frame
	(function () ->
           exec_with_temporary_breakpoint
             pc
             (function () ->
                while
                  run ();
     	          match current_report () with
     	            Some
                      {Rep_type = Rep_breakpoint;
                       Rep_stack_pointer = sp;
                       Rep_program_pointer = pc2}
                  ->
     	            (pc = pc2) && (frame <> sp)
                  | _ ->
     	            false
                do
     	          ()
     	        done));;

(* --- We can't rely on frame 0 as local variables are *)
(* --- put on the return stack. *)
let next_1 () =
  match current_report () with
    None ->				(* Beginning of the program. *)
      step 1
  | Some {Rep_stack_pointer = sp1} ->	(* Others cases. *)
      let frame_info =  move_frame 1 in
        step 1;
	if not !interrupted then
	  match move_frame 1 with	(* Call `finish' if we have *)
	    None -> ()			(* entered a function. *)
	  | Some (frame2, _) ->		(* --- Should work in most cases... *)
	      match frame_info with
		None -> finish ()
	      | Some (frame1, _) ->
	          if frame1 < frame2 then
	            finish ();;

(* Same as `step' (forward) but skip over function calls. *)
let rec next =
  function
    0 -> ()
  | n ->
      next_1 ();
      if not !interrupted then
        next (n - 1);;
