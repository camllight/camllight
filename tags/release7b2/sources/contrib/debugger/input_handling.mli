(***************************** Input control *****************************)

#open "primitives";;

(*** Actives files. ***)

(* Add a file to the list of active files. *)
value add_file : IO_CHANNEL -> (IO_CHANNEL -> unit) -> unit;;

(* Remove a file from the list of actives files. *)
value remove_file : IO_CHANNEL -> unit;;

(* Change the controller for the given file. *)
value change_controller : IO_CHANNEL -> (IO_CHANNEL -> unit) -> unit;;

(* Return the controller currently attached to the given file. *)
value current_controller : IO_CHANNEL -> (IO_CHANNEL -> unit);;

(* Execute a function with `controller' attached to `file'. *)
(* ### controller file funct *)
value execute_with_other_controller :
  (IO_CHANNEL -> unit) -> IO_CHANNEL -> (unit -> 'a) -> 'a;;

(*** The "Main Loop" ***)

(* Call this function for exiting the main loop. *)
value exit_main_loop : 'a -> unit;;

(* Handle active files until `continue_main_loop' is false. *)
value main_loop : unit -> unit;;

(*** Managing user inputs ***)

(* Are we in interactive mode ? *)
value interactif : bool ref;;

value current_prompt : string ref;;

(* Where the user input come from. *)
value user_channel : IO_CHANNEL ref;;

value read_user_input : string -> int -> int;;

(* Stop reading user input. *)
value stop_user_input : unit -> unit;;

(* Resume reading user input. *)
value resume_user_input : unit -> unit;;

(* Ask user a yes or no question. *)
value yes_or_no : string -> bool;;
