(***************************** Checkpoints *******************************)

#open "primitives";;
#open "communication";;

(*** A type for checkpoints. ***)

type CHECKPOINT_STATE = C_stopped | C_running of int;;

(* `C_valid' is true if and only if the corresponding *)
(* process is connected to the debugger. *)
(* `C_parent' is the checkpoint whose process is parent *)
(* of the checkpoint one (`root' if no parent). *)
(* C_pid = 2 for root pseudo-checkpoint. *)
(* C_pid = -1 for kill checkpoints. *)
(* C_pid = 0 for ghost checkpoints. *)
type CHECKPOINT =
  {mutable C_time : int;
   mutable C_pid : int;
   mutable C_fd : IO_CHANNEL;
   mutable C_valid : bool;
   mutable C_report : REPORT option;
   mutable C_state : CHECKPOINT_STATE;
   mutable C_parent : CHECKPOINT;
   mutable C_breakpoint_version : int;
   mutable C_breakpoints : (int * (int ref * char)) list;
   mutable C_trap_barrier : int};;

(*** Pseudo-checkpoint `root'. ***)
(* --- Parents of all checkpoints which have no parent. *)
value root : CHECKPOINT;;

(*** Current state ***)
value checkpoints : CHECKPOINT list ref;;
value current_checkpoint : CHECKPOINT ref;;

value current_time : unit -> int;;
value current_report : unit -> REPORT option;;
value current_pc : unit -> int option;;
