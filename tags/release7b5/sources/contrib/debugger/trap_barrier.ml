(***************************** Trap barrier ********************************)

#open "communication";;
#open "checkpoints";;
#open "interruption";;

let current_trap_barrier = ref 0;;

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

(* Execute `funct' with a trap barrier. *)
(* --- Used by `finish'. *)
let exec_with_trap_barrier trap_barrier funct =
  try
    install_trap_barrier trap_barrier;
    funct ();
    remove_trap_barrier ()
  with
    x ->
      remove_trap_barrier ();
      raise x;;
