(* Primitives provided in camltk.c *)
#open "unix";;

value install_callback_handler : (string list -> unit) -> unit
      	= 1 "camltk_install_callback_handler"
  and opentk : string -> unit
        = 1 "camltk_opentk"
  and tcl_eval : string -> string
      	= 1 "camltk_tcl_eval"
  and add_file_input : file_descr -> string -> unit
        = 2 "camltk_add_file_input"
  and rem_file_input : file_descr -> unit
        = 1 "camltk_rem_file_input"
  and tk_mainloop : unit -> unit
      	= 1 "camltk_tk_mainloop"
;;
