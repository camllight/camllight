(******************************* Frames ************************************)

#open "lambda";;
#open "communication";;
#open "checkpoints";;
#open "events";;

(* Current frame number *)
let current_frame = ref 0;;

(* Program counter of current frame. *)
let selected_program_counter = ref 0;;

(* List of events at selected position. *)
let selected_events = ref ([] : event list);;

(* The main event of the previous list *)
(* --- Cf `main_event' for definition of "main event". *)
let selected_event = ref (None : event option);;

(* Recompute the data above. *)
(* --- Assume the currents events have already been updated. *)
let update_frame () =
  if !current_frame = 0 then
    (selected_events := !current_events;
     selected_event := !current_event)
  else
    (selected_events := events_at_pc !selected_program_counter;
     selected_event :=
       try
       	 Some (main_event !selected_events)
       with
       	 Not_found ->
	   None);;

(* Selected position in source. *)
(* Raise `Not_found' if not on an event. *)
let selected_point () =
  match !selected_event with
    None ->
      raise Not_found
  | Some {ev_char = point; ev_file = module} ->
      (module, point);;

let selected_event_is_before () =
  match !selected_event with
    None ->
      raise Not_found
  | Some {ev_kind = Lbefore} ->
      true
  | _ ->
      false;;

(* Select a frame. *)
(* Raise `Not_found' if no such frame. *)
(* --- Assume the currents events have already been updated. *)
let select_frame frame_number =
  if current_pc () = None then
    raise Not_found;
  match move_frame frame_number with
    None ->
      raise Not_found
  | Some (frame, pc) ->
      current_frame := frame_number;
      selected_program_counter := pc;
      update_frame ();;

(* Select a frame. *)
(* Same as `select_frame' but raise no exception if the frame is not found. *)
(* --- Assume the currents events have already been updated. *)
let try_select_frame frame_number =
  try
    select_frame frame_number
  with
    Not_found ->
      ();;

(* Return to default frame (frame 0). *)
let reset_frame () =
  try_select_frame 0;;

let stack_depth () =
  let rec find_depth n =
    try
      select_frame n;
      find_depth (n + 1)
    with
      Not_found -> n
  in
    find_depth 0;;
