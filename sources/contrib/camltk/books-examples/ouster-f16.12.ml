#open "tk";;


let top = OpenTk() in

 let scales = 
    map (function label ->
           scale__create top
       	    [Label label; From 0; To 255; Length (Centimeters 10.); 
            Orientation Horizontal])
  ["Red"; "Green"; "Blue"] 

 and sample =
     frame__create top [Height (Centimeters 1.5); Width (Centimeters 6.)] in

 pack scales [Side Side_Top];
 pack [sample] [Side Side_Bottom; PadY (Millimeters 2.)];

 let newColor _ =
   let rgb_int = map scale__get scales in
   let [r;g;b] = map (format_int "%02x") rgb_int in
   let color = "#"^r^g^b in
   frame__configure sample [Background (NamedColor color)] in
 do_list (function s -> 
      	    scale__configure s [Slidecommand newColor]) scales;
 MainLoop()
;;
