(* Display information about the current event. *)
value show_current_event : unit -> unit;;

(* Display information about the current frame. *)
(* --- `select frame' must have succeded before calling this function. *)
value show_current_frame : bool -> unit;;
