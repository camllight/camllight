#open "tk";;
#open "frx_misc";;

let rec mapi f n l =
  match l with
    [] -> [] 
  | x::l -> let v = f n x in v::(mapi f (succ n) l)
;;

(* Same as tk_dialog, but not sharing the tkwait variable *)
(* w IS the parent widget *)
let f w name title mesg bitmap def buttons =
  let t = toplevelw__create_named w name [Class "Dialog"] in
    wm__title_set t title;
    wm__iconname_set t "Dialog";
    wm__protocol_set t "WM_DELETE_WINDOW" (function () -> ());
    wm__transient_set t (winfo__toplevel w);
  let ftop = 
   frame__create_named t "top" [Relief Raised; BorderWidth (Pixels 1)]
  and fbot =
   frame__create_named t "bot" [Relief Raised; BorderWidth (Pixels 1)]
   in
     pack [ftop][Side Side_Top; Fill Fill_Both];
     pack [fbot][Side Side_Bottom; Fill Fill_Both];

  let l =
   label__create_named ftop "msg" 
     [Justify Justify_Left; Text mesg; WrapLength (Pixels 600)] in
     pack [l][Side Side_Right; Expand true; Fill Fill_Both;
       	      PadX (Millimeters 3.0); PadY (Millimeters 3.0)];
  begin match bitmap with
     Predefined "" -> ()
  |  _ ->
    let b = 
      label__create_named ftop "bitmap" [Bitmap bitmap] in
     pack [b][Side Side_Left; PadX (Millimeters 3.0); PadY (Millimeters 3.0)]
  end;
  
  let waitv = textvariable__create_temporary t in
 
  let buttons =
    mapi (fun i bname ->
     let b = button__create t 
      	      [Text bname; 
      	       Command (fun () -> textvariable__set waitv (string_of_int i))] in
    if i = def then begin
      let f = frame__create_named fbot "default" 
      	         [Relief Sunken; BorderWidth (Pixels 1)] in
        raise_window_above b f;
	pack [f][Side Side_Left; Expand true; 
      	         PadX (Millimeters 3.0); PadY (Millimeters 2.0)];
        pack [b][In f; PadX (Millimeters 2.0); PadY (Millimeters 2.0)];
	bind t [[], KeyPressDetail "Return"]
	 (BindSet ([], (fun _ -> button__flash b; button__invoke b)))
	end
    else
      pack [b][In fbot; Side Side_Left; Expand true; 
      	       PadX (Millimeters 3.0); PadY (Millimeters 2.0)];
    b
    )
    0 buttons in

   wm__withdraw t;
   update_idletasks();
   let x = (winfo__screenwidth t)/2 - (winfo__reqwidth t)/2 -
             (winfo__vrootx (winfo__parent t))
   and y = (winfo__screenheight t)/2 - (winfo__reqheight t)/2 -
             (winfo__vrooty (winfo__parent t)) in
   wm__geometry_set t ("+"^string_of_int x^"+"^string_of_int y);
   wm__deiconify t;

   let oldfocus = try Some (focus__get()) with _ -> None
   and oldgrab = grab__current t
   and grabstatus = ref None in
    begin match oldgrab with 
      [] -> ()
    | x::l -> grabstatus := Some(grab__status x)
    end;
  
   grab__set t;
   tkwait__visibility t;
   focus__set (if def >= 0 then nth buttons def else t);

   tkwait__variable waitv;
   begin match oldfocus with
       None -> ()
     | Some w -> try focus__set w with _ -> ()
   end;
   destroy t;
   begin match oldgrab with
     [] -> ()
   | x::l -> 
      match !grabstatus with
       	Some(GrabGlobal) -> grab__set_global x
      | _ -> grab__set x
   end;

   int_of_string (textvariable__get waitv)
;;
