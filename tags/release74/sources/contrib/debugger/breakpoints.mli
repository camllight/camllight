(******************************* Breakpoints ***************************)

#open "lambda";;

(*** Debugging. ***)
value debug_breakpoints : bool ref;;

(*** Information about breakpoints. ***)

value breakpoints_count : unit -> int;;

(* Breakpoint number -> event. *)
value breakpoints : (int * event) list ref;;

(* `None' if no breakpoint at `pc'; `Some instruction' otherwise. *)
value instruction_at_pc : int -> char option;;

(* Is there a breakpoint at `pc' ? *)
value breakpoint_at_pc : int -> bool;;

(* List of breakpoints at `pc'. *)
value breakpoints_at_pc : int -> int list;;

(*** Set and remove breakpoints ***)

(* Ensure the current version in installed in current checkpoint. *)
value update_breakpoints : unit -> unit;;

(* Execute given function with no breakpoint in current checkpoint. *)
(* --- `goto' run faster so (does not stop on each breakpoint). *)
value execute_without_breakpoints : (unit -> unit) -> unit;;

(* Insert a new breakpoint in lists. *)
value new_breakpoint : event -> unit;;

(* Remove a breakpoint from lists. *)
value remove_breakpoint : int -> unit;;

value remove_all_breakpoints : unit -> unit;;

(*** Temporary breakpoints. ***)

(* Temporary breakpoint position. *)
value temporary_breakpoint_position :int option ref;;

(* Execute `funct' with a breakpoint added at `pc'. *)
(* --- Used by `finish'. *)
value exec_with_temporary_breakpoint : int -> (unit -> unit) -> unit;;
