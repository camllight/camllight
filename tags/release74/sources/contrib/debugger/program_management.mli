(*** Program loading and initializations. ***)

value loaded : bool ref;;
value ensure_loaded : unit -> unit;;

(*** Kill program. ***)
value kill_program : unit -> unit;;

(* Ask wether to kill the program or not. *)
(* If yes, kill it. *)
(* Return true iff the program has been killed. *)
value ask_kill_program : unit -> bool;;
