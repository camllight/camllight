#open "animation";;

let sort gc =
  let rec loop1 i =
    if i >= vect_length gc.array then Finished else
    let val_i = gc.array.(i) in
    let rec loop2 j =
      if j < 1 then begin
        assign gc j val_i;
        loop1 (i+1)
      end else
        Pause(fun () ->
          if val_i >= gc.array.(j-1) then begin
            assign gc j val_i;
            loop1 (i+1)
          end else begin
            assign gc j gc.array.(j-1);
            loop2 (j-1)
          end)
    in loop2 i
  in loop1 1
;;
