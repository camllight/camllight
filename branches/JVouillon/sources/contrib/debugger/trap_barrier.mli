(***************************** Trap barrier ********************************)

#open "checkpoints";;

(* Is trap barrier currently set ? *)
value trap_barrier : unit -> bool;;

value install_trap_barrier : int -> unit;;

value remove_trap_barrier : unit -> unit;;

(* Ensure the trap barrier state is up to date in current checkpoint. *)
value update_trap_barrier : unit -> unit;;
