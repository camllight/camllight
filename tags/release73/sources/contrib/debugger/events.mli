#open "lambda";;

(*** Events list. ***)
value events : event list ref;;

(* Modules used by the program. *)
value modules : string list ref;;

(* Load the event list. *)
value load_events : in_channel -> unit;;

(*** Utilities. ***)

(* Return the main event of the event list. *)
(* --- It is the element of the list, `Lafter' if some are, *)
(* --- whose position in the source is the greatest. *)
(* Raise `Not_found' if no such event (the list is empty). *)
value main_event : event list -> event;;

(* Return the list of events at `pc'. *)
value events_at_pc : int -> event list;;

(* Return the main event at `pc'. *)
value event_at_pc : int -> event;;

(** Current events. **)

(* List of events at current position. *)
value current_events : event list ref;;

(* The main event of the previous list. *)
(* --- It is the element of the list, `Lafter' if some are, *)
(* --- whose position in the source is the greatest. *)
value current_event : event option ref;;

(* Recompute the data above. *)
value update_current_event : unit -> unit;;

(* Current position in source. *)
(* Raise `Not_found' if not on an event (beginning or end of program). *)
value current_point : unit -> string * int;;

value current_event_is_before : unit -> bool;;

(** Finding events. **)

(* List the events in `module'. *)
(* ### module -> event_list *)
value events_in_module : string -> event list;;

(* First event after the given position. *)
(* --- Raise `Not_found' if no such event. *)
value event_after_pos : string -> int -> event;;

(* Nearest event from given position. *)
(* --- Raise `Not_found' if no such event. *)
value event_near_pos : string -> int -> event;;
