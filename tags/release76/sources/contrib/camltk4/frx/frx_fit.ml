#open "tk";;
#open "frx_misc";;

let debug = ref false
;;

let vert wid =
  let newsize = ref 0
  and pending_resize = ref false in
  let rec resize () =
    pending_resize := false;
    if !debug then (printf__eprintf "Resize %d\n" !newsize; flush stderr);
    text__configure wid [TextHeight !newsize];
    let _ = add_timer 100 (fun () -> frx_after__idle check) in
    ()
  and check () = 
    let first, last = text__yview_get wid in 
      check1 first last

  and check1 first last =
    let curheight = int_of_string (cget wid CHeight) in
      if !debug then begin
      	 printf__eprintf "C %d %f %f\n" curheight first last; flush stderr
	 end;
      if first = 0.0 && last = 1.0 then ()
      (* Don't attempt anything if widget is not visible *)
      else if not (winfo__viewable wid) then begin
        if !debug then (printf__eprintf "C notviewable\n"; flush stderr);
	(* Try again later *)
      	bind wid [[], Expose] (BindSet ([], fun _ ->
	       bind wid [[], Expose] BindRemove;
	       check()))
        end
      else  begin
	let delta = 
	  if last = 0.0 then 1
	  else 
	    let visible = max 0.1 (last -. first) in
	    max 1 (truncate (float curheight *. (1. -. visible))) in
        newsize := max (curheight + delta) !newsize;
	if !debug then (printf__eprintf "newsize: %d\n" !newsize; flush stderr);
	if !pending_resize then ()
	else begin
	  pending_resize := true;
	  frx_after__idle resize
	  end
        end

    and scroll first last =
      if !debug then (printf__eprintf "V %f %f\n" first last; flush stderr);
      if first = 0.0 && last = 1.0 then ()
      else check1 first last
    in
      scroll, check
;;
