(***************************** Trap barrier ********************************)

value install_trap_barrier : int -> unit;;

value remove_trap_barrier : unit -> unit;;

(* Ensure the trap barrier state is up to date in current checkpoint. *)
value update_trap_barrier : unit -> unit;;

(* Execute `funct' with a trap barrier. *)
(* --- Used by `finish'. *)
value exec_with_trap_barrier : int -> (unit -> unit) -> unit;;
