(***************************** Checkpoints *******************************)

#open "primitives";;
#open "communication";;

(*** Pseudo-checkpoint `root'. ***)
(* --- Parents of all checkpoints which have no parent. *)
let rec root =
  {C_time = 0;
   C_pid = -2;
   C_fd = std_io;
   C_valid = false;
   C_report = None;
   C_state = C_stopped;
   C_parent = root;
   C_breakpoint_version = 0;
   C_breakpoints = [];
   C_trap_barrier = 0};;

(*** Current state ***)
let checkpoints =
  ref ([] : CHECKPOINT list);;

let current_checkpoint =
  ref root;;

let current_time () =
  !current_checkpoint.C_time;;

let current_report () =
  !current_checkpoint.C_report;;

let current_pc () =
  match current_report () with
    None | Some {Rep_type = Rep_exited | Rep_exc} -> None
  | Some {Rep_program_pointer = pc } -> Some pc;;
