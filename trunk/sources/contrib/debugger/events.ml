(******************************** Events ******************************)

#open "lambda";;
#open "primitives";;
#open "checkpoints";;

(*** Events list. ***)
let events = ref ([] : event list);;

let events_by_pc = (hashtbl__new 101 : (int, event) hashtbl__t);;

(* Modules used by the program. *)
let modules = ref ([] : string list);;

(* Previous `pc'. *)
(* Save time if `update_current_event' is called *)
(* several times at the same point. *)
let old_pc = ref (None : int option);;

(* Load the event list. *)
let load_events inchan =
  old_pc := None;
  events := input_value inchan;
  hashtbl__clear events_by_pc;
  let module_set = ref(set__empty compare_strings) in
    do_list
      (function ev ->
          hashtbl__add events_by_pc ev.ev_pos ev;
          module_set := set__add ev.ev_file !module_set)
      !events;
    modules := set__elements !module_set;;

(*** Utilities. ***)

(* Return the main event of the event list. *)
(* --- It is the element of the list, `Lafter' if some are, *)
(* --- whose position in the source is the greatest. *)
(* Raise `Not_found' if no such event (the list is empty). *)
let main_event event_list =
  let event_after =
    filter
      (function {ev_kind = Lafter _} -> true | _ -> false)
      event_list
  in
    let rec find_max pos_m m =
      function
	[] ->
      	  m
      | ({ev_char = pos} as m')::l ->
	  if pos > pos_m then
	    find_max pos m' l
	  else
	    find_max pos_m m l
    in
      match
        if event_after = [] then event_list else event_after
      with
	[] ->
      	  raise Not_found
      | ({ev_char = pos} as m)::l ->
	  find_max pos m l;;

(* Return the list of events at `pc' *)
let events_at_pc =
  hashtbl__find_all events_by_pc;;

(* Return the main event at `pc' *)
let event_at_pc pc =
  main_event (events_at_pc pc);;

(*** Current events. ***)

(* List of events at current position. *)
let current_events =
  ref ([] : event list);;

(* The main event of the previous list. *)
let current_event = 
  ref (None : event option);;

(* Recompute the data above. *)
let update_current_event () =
  match current_pc () with
    None ->
      old_pc := None;
      current_events := [];
      current_event := None
  | (Some pc) as opt_pc ->
      if opt_pc <> !old_pc then
      	(current_events := events_at_pc pc;
      	 current_event := Some (main_event !current_events);
      	 old_pc := opt_pc);;

(* Current position in source. *)
(* Raise `Not_found' if not on an event (beginning or end of program). *)
let current_point () =
  match !current_event with
    None ->
      raise Not_found
  | Some {ev_char = point; ev_file = module} ->
      (module, point);;

let current_event_is_before () =
  match !current_event with
    None ->
      raise Not_found
  | Some {ev_kind = Lbefore} ->
      true
  | _ ->
      false;;

(*** Finding events. ***)

(* List the events in `module'. *)
let events_in_module module =
  filter
    (function {ev_file = m} -> module = m)
    !events;;

(* First event after the given position. *)
(* Raise `Not_found' if no such event. *)
let event_after_pos module position =
  match
    list_it
      (function
      	 ({ev_char = pos1} as ev) ->
	   if pos1 < position then
	     function x -> x
	   else
	     function
	       None ->
	         Some ev
             | (Some {ev_char = pos2} as old) ->
	         if pos1 < pos2 then
		   Some ev
		 else
		   old)
      (events_in_module module)
      None
  with
    None ->
      raise Not_found
  | Some x ->
      x;;

(* Nearest event from given position. *)
(* Raise `Not_found' if no such event. *)
let event_near_pos module position =
  match events_in_module module with
    [] ->
      raise Not_found
  | [event] ->
      event
  | a::l ->
      list_it
      	(fun ({ev_char = pos1} as ev) ({ev_char = pos2} as old) ->
	   if abs (position - pos1) < abs (position - pos2) then
	     ev
	   else
	     old)
	l
	a;;

