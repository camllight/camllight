(***************************** Trap barrier ********************************)

#open "communication";;
#open "checkpoints";;
#open "interruption";;

let current_trap_barrier = ref 0;;

(* Is trap barrier currently set ? *)
let trap_barrier () =
  !current_trap_barrier <> 0;;

let install_trap_barrier pos =
  current_trap_barrier := pos;;

let remove_trap_barrier () =
  current_trap_barrier := 0;;

(* Ensure the trap barrier state is up to date in current checkpoint. *)
let update_trap_barrier () =
  if !current_checkpoint.C_trap_barrier <> !current_trap_barrier then
    exec_protected
      (function () ->
         set_trap_barrier !current_trap_barrier;
         !current_checkpoint.C_trap_barrier <- !current_trap_barrier);;
