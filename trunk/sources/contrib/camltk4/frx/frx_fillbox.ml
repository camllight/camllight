#open "tk";;

(*
 * Indicator
 *)

let new_vertical parent w h =
  let f = frame__create parent 
      	    [Width (Pixels w); Height (Pixels h); 
      	     Relief Sunken; Borderwidth (Millimeters 1.0)] in
  let ffill = frame__create f 
      	       [Width (Pixels w); Height (Pixels 0); Background Blue]
  and fempty = frame__create f [Width (Pixels w); Height (Pixels h)] in
     pack[ffill][Side Side_Bottom; Fill Fill_X];
     pack[fempty][Side Side_Top; Fill Fill_Both];

  f, (function percent ->
      	let hf = percent*h/100 in
      	frame__configure ffill [Height (Pixels hf)];
      	frame__configure fempty [Height (Pixels (h - hf))])
;;
