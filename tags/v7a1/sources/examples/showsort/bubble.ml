#open "animation";;

let sort gc =
  let ordered = ref true in
  let rec sweep i =
    if i+1 >= vect_length gc.array then
      if !ordered then
        Finished
      else begin
        ordered := false;
        sweep 0
      end
    else
      Pause(fun () ->
        if gc.array.(i+1) < gc.array.(i) then begin
          exchange gc i (i+1);
          ordered := false
        end;
        sweep(i+1))
  in sweep 0
;;
