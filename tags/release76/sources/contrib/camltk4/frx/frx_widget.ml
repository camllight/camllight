#open "tk";;

let version = "$Id$"
;;

(* Make a window (toplevel widget) resizeable *)
let resizeable t =
  update_idletasks(); (* wait until layout is computed *)
  wm__minsize_set t (winfo__width t) (winfo__height t)
;;
(*
let configure =
  let table = Hashtbl.new 17 in
    List.iter (function class, f ->
      	       Hashtbl.add table class f)
	["button", Button.configure;
	 "canvas", Canvas.configure;
	 "checkbutton", Checkbutton.configure;
	 "entry", Entry.configure;
	 "frame", Frame.configure;
	 "label", Label.configure;
	 "listbox", Listbox.configure;
	 "menu", Menu.configure;
	 "menubutton", Menubutton.configure;
	 "message", Message.configure;
	 "radiobutton", Radiobutton.configure;
	 "scale", Scale.configure;
	 "scrollbar", Scrollbar.configure;
	 "text", Text.configure;
	 "toplevel", Toplevelw.configure;
        ];
   fun w ol ->
     try
       Hashtbl.find table (Support.widget_class  w) w ol
     with
       Not_found -> raise (Invalid_argument "configure")
*)
