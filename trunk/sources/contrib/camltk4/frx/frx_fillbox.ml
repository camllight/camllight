#open "tk";;

(*
 * Progress indicators
 *)

let new_vertical parent w h =
  let f = frame__create parent 
      	    [Width (Pixels w); Height (Pixels h); 
      	     Relief Sunken; BorderWidth (Millimeters 1.0)] in
  let initfill = [Width (Pixels w); Height (Pixels 0); Background Green]
  and initempty = [Width (Pixels w); Height (Pixels h)] in
  let ffill = frame__create f initfill
  and fempty = frame__create f initempty in
     pack[ffill][Side Side_Bottom; Fill Fill_X];
     pack[fempty][Side Side_Top; Fill Fill_Both];

  f, (function percent ->
      	match percent with
	   0 -> frame__configure ffill initfill;
	        frame__configure fempty initempty
         | -1 -> frame__configure ffill [Background Red]
         | _ -> let hf = percent*h/100 in
		  frame__configure ffill [Height (Pixels hf)];
		  frame__configure fempty [Height (Pixels (h - hf))])
;;

let new_horizontal parent w h =
  let f = frame__create parent 
      	    [Width (Pixels w); Height (Pixels h); 
      	     Relief Sunken; BorderWidth (Millimeters 1.0)] in
  let initfill = [Width (Pixels 0); Height (Pixels h); Background Green]
  and initempty = [Width (Pixels w); Height (Pixels h)] in
  let ffill = frame__create f initfill
  and fempty = frame__create f initempty in
     pack[ffill][Side Side_Left; Fill Fill_Y];
     pack[fempty][Side Side_Right; Fill Fill_Both];

  f, (function percent ->
      	match percent with
           0 -> frame__configure ffill initfill;
	        frame__configure fempty initempty
	 | -1 -> frame__configure ffill [Background Green]
         | _ -> let hf = percent*h/100 in
		  frame__configure ffill [Width (Pixels hf)];
		  frame__configure fempty [Width (Pixels (h - hf))])
;;

