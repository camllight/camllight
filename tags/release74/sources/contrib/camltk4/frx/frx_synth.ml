(* Some notion of synthetic events *)
#open "protocol";;

(* To each event is associated a table of (widget, callback) *)
let events = hashtblc__new 37
;;

(* Notes:
 *   "cascading" events (on the same event) are not supported 
 *   Only one binding active at a time for each event on each widget.
 *)

(* Get the callback table associated with <name>. Initializes if required *)
let get_event name =
  try hashtblc__find events name 
  with
    Not_found ->
      let h = hashtblc__new 37 in
       hashtblc__add events name h;
       (* Initialize the callback invocation mechanism, based on 
          variable trace
        *)
       let rec set () =
	protocol__var_handle ("camltk_events(" ^ name ^")")
	 (fun () ->
	    begin match tkEval [| TkToken "set";
			    TkToken ("camltk_events(" ^ name ^")") |] with
	      "all" -> (* Invoke all callbacks *)
		hashtblc__do_table_rev 
      	       	  (fun p f -> 
      	       	     try 
      	       	      f (cTKtoCAMLwidget p) 
      	       	     with _ -> ())
                  h
	    | p -> (* Invoke callback for p *)
		try
		  let w = cTKtoCAMLwidget p
      	       	  and f = hashtblc__find h p in
		    f w
      	        with
      	       	  _ -> ()
            end; 
      	    set ()(* reactivate the callback *)
            ) in
       set();
       h 
;;
(* Remove binding for event <name> on widget <w> *)
let remove w name =   
  hashtblc__remove (get_event name) (support__widget_name w)
;;
(* Adds <f> as callback for widget <w> on event <name> *)
let bind w name f =
  remove w name;
  hashtblc__add (get_event name) (support__widget_name w) f
;;
(* Sends event <name> to all widgets *)
let broadcast name =
  tkDo [| TkToken "set"; 
          TkToken ("camltk_events(" ^ name ^")");
	  TkToken "all" |]
;;
(* Sends event <name> to widget <w> *)
let send name w =
  tkDo [| TkToken "set"; 
          TkToken ("camltk_events(" ^ name ^")");
	  TkToken (support__widget_name w) |]
;;
(* Remove all callbacks associated to widget <w> *)
let remove_callbacks w =
  hashtblc__do_table (fun _ h -> hashtblc__remove h (support__widget_name w)) events
;;
let _ =
  add_destroy_hook remove_callbacks
;;
