(******************************* Frames ************************************)

#open "lambda";;

(* Current frame number *)
value current_frame : int ref;;

(* Program counter of current frame. *)
value selected_program_counter : int ref;;

(* List of events at selected position. *)
value selected_events : event list ref;;

(* The main event of the previous list *)
(* --- Cf `main_event' for definition of "main event". *)
value selected_event : event option ref;;

(* Selected position in source. *)
(* Raise `Not_found' if not on an event. *)
value selected_point : unit -> string * int;;

value selected_event_is_before : unit -> bool;;

(* Select a frame. *)
(* Raise `Not_found' if no such frame. *)
(* --- Assume the currents events have already been updated. *)
value select_frame : int -> unit;;

(* Select a frame. *)
(* Same as `select_frame' but raise no exception if the frame is not found. *)
(* --- Assume the currents events have already been updated. *)
value try_select_frame : int -> unit;;

(* Return to default frame (frame 0). *)
value reset_frame : unit -> unit;;

value stack_depth : unit -> int;;
