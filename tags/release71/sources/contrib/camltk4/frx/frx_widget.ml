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
  let table = hashtbl__new 17 in
    do_list (function class, f ->
      	       hashtbl__add table class f)
	["button", button__configure;
	 "canvas", canvas__configure;
	 "checkbutton", checkbutton__configure;
	 "entry", entry__configure;
	 "frame", frame__configure;
	 "label", label__configure;
	 "listbox", listbox__configure;
	 "menu", menu__configure;
	 "menubutton", menubutton__configure;
	 "message", message__configure;
	 "radiobutton", radiobutton__configure;
	 "scale", scale__configure;
	 "scrollbar", scrollbar__configure;
	 "text", text__configure;
	 "toplevel", toplevelw__configure;
        ];
   fun w ol ->
     try
       hashtbl__find table (support__widget_class  w) w ol
     with
       Not_found -> raise (Invalid_argument "configure")
;;
*)
