#open "checkpoints";;
#open "misc";;
#open "primitives";;
#open "debugger_config";;

let history = ref ([] : int list);;

let empty_history () =
  history := [];;

let add_current_time () =
  let time = current_time () in
    if history = ref [] then
      history := [time]
    else
      if time <> hd !history then
        history := list_truncate !history_size (time::!history);;

let previous_time_1 () =
  match !history with
    _::((time::_) as hist) ->
      history := hist; time
  | _ ->
      prerr_endline "No more information."; raise Toplevel;;

let repeat funct =
  let rec rep =
    function
      1 -> funct ()
    | n -> let _ = funct () in rep (n - 1)
  in
    rep;;

let previous_time n = repeat previous_time_1 n;;
