#open "unix";;
#open "tk";;

let buttons = (queue__new () : support__Widget queue__t)
;;

let top = OpenTkClass "redo" in
let v_cmd = textvariable__new () in
let e = entry__create top [TextWidth 30; Relief Sunken;
      	       	       	   TextVariable v_cmd] in

pack [e] [PadX (Millimeters 1.); PadY (Millimeters 1.)];
tk__bind e [[], XKey "Return"]
  (BindSet ([],
      (function _ ->
      	if queue__length buttons >= 5 then destroy (queue__take buttons);
      	let cmd = textvariable__get v_cmd  in
        let b = button__create top
	         [Command (function () -> system cmd; ());
                  Text cmd] in
	 queue__add b buttons;
         pack [b][Fill Fill_X];
         button__invoke b;
	 entry__delete e (Number 0) End)));
MainLoop();;

	 
