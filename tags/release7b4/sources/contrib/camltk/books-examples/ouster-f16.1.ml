#open"tk";;

let top = OpenTk() in
  do_list (function (r,otheropts) ->
      	    let f = frame__create top 
      	       	       ([Width (Millimeters 15.);
	                 Height (Millimeters 10.);
			 Relief r;
			 Borderwidth (Pixels 4)] @ otheropts) in
                pack [f] [Side Side_Left; 
      	       	       	  PadX (Millimeters 2.);
      	       	       	  PadY (Millimeters 2.)])
   
       	   [Raised, [];
	    Sunken, [];
	    Flat, [Background Black];
      	    Groove,[];
	    Ridge,[]];
   MainLoop()
;;

	    
