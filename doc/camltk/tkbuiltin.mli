(* Tk initialisation functions, event bindings, extensions *)

value OpenTk : unit -> Widget
and   OpenTkClass : string -> Widget
      (* Initialisation of the Tk interface, should be called only once.
      	 Both functions will return the toplevel widget.
	 [OpenTkClass "class"] lets you specify the (resource) class 
      	 of the toplevel widget *)
;;

value MainLoop : unit -> unit 
      (* Enter the main loop for user interaction *)
;;

value CloseTk : unit -> unit
      (* Terminates the user-interface interaction *)
;;


value bind : Widget -> (Modifier list * XEvent) list -> BindAction -> unit
      (* Binding events to widgets *)
;;

value canvas_bind : Widget -> TagOrId 
     -> (Modifier list * XEvent) list -> BindAction -> unit
      (* Binding events to canvas objects *)
;;

value text_tag_bind : Widget -> string 
     -> (Modifier list * XEvent) list -> BindAction -> unit
      (* Binding events to tags in text widgets *)
;;

value add_fileinput : file_descr -> (unit -> unit) -> unit
and remove_fileinput : file_descr -> unit
      (* Callbacks on Unix file descriptors *)
;;
