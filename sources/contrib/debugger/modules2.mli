(********************** More code for modules ******************************)

(* Initialization : open the default modules. *)
value initialize_modules : string list -> unit;;

(* Close all modules. *)
value close_all_modules : unit -> unit;;

(* Open a module. *)
value open_module : string -> unit;;

(* Close a module. *)
value close_module : string -> unit;;
