#open "primitives";;

(*** Debugging. ***)

value debug_loading : bool ref;;

(*** Load program ***)

(* Function used for launching the program. *)
value launching_function : (unit -> unit) ref;;

(* Should we enable SIGINT in debugged programs ? *)
(* --- no if they are childs of the debugger *)
(* --- (otherwise, we can't interrupt the program (it get killed)). *)
value enable_sigint : bool ref;;

value load_program : unit -> unit;;

type LAUNCHING_FUNCTION == bool * (unit -> unit);;

value loading_modes : (string * LAUNCHING_FUNCTION) list;;
value set_launching_function : LAUNCHING_FUNCTION -> unit;;

(** Connection **)
value connection : IO_CHANNEL ref;;
value connection_opened : bool ref;;
