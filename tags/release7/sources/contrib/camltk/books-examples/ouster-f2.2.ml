#open "tk";;

let power base p =
  let rec pow accu = function 
      0 -> accu
    | n -> pow (accu * base) (pred n) in
   pow 1 p
;;

let powers b p =
  try
   string_of_int (power (int_of_string b) (int_of_string p))
  with
   _ -> "error"
;;


let top = OpenTk() in
let v_base = textvariable__new () 
and v_power = textvariable__new () 
and v_result = textvariable__new () in

let e_base = 
  entry__create top [TextWidth 6; Relief Sunken; TextVariable v_base]
and l1 = 
  label__create top [Text "to the power"]
and e_power = 
  entry__create top [TextWidth 6; Relief Sunken; TextVariable v_power]
and l2 =
  label__create top [Text "is"]
and lresult =
  label__create top [TextVariable v_result] in

let compute _ = textvariable__set v_result
			      (powers (textvariable__get v_base)
			              (textvariable__get v_power)) in

  pack [e_base; l1; e_power; l2; lresult] [Side Side_Left;
      	       	       	       	       	   PadX (Millimeters 1.);
      	       	       	       	       	   PadY (Millimeters 2.)];

  bind e_base [[], XKey "Return"] (BindSet ([], compute));
  bind e_power [[], XKey "Return"] (BindSet ([], compute));
  MainLoop()
;;
