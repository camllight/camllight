(**************************** Time travel ***********************)

#open "primitives";;

exception Current_checkpoint_lost;;

value new_checkpoint : int -> IO_CHANNEL -> unit;;
value set_file_descriptor : int -> IO_CHANNEL -> bool;;
value kill_all_checkpoints : unit -> unit;;
value forget_process : IO_CHANNEL -> int -> unit;;
value recover : unit -> unit;;

value go_to : int -> unit;;
value run : unit -> unit;;
value back_run : unit -> unit;;
value step : int -> unit;;
value finish : unit -> unit;;
value next : int -> unit;;
