(****************** Miscellaneous parameters *******************************)

value program_name : string ref;;
value socket_name : string ref;;
value arguments : string ref;;
value default_load_path : string list ref;;

value add_path : string -> unit;;

(* Used by emacs ? *)
value emacs : bool ref;;
