(* Primitives provided in camltk.c *)
#open "unix";;

type TkTimer;;

value install_callback_handler : (int * string list -> unit) -> unit
      	= 1 "camltk_install_callback_handler"
  and opentk : string -> string -> unit
        = 2 "camltk_opentk"
  and tcl_eval : string -> string
      	= 1 "camltk_tcl_eval"
  and add_file_input : file_descr -> int -> unit
        = 2 "camltk_add_file_input"
  and rem_file_input : file_descr -> unit
        = 1 "camltk_rem_file_input"
  and add_file_output : file_descr -> int -> unit
        = 2 "camltk_add_file_output"
  and rem_file_output : file_descr -> unit
        = 1 "camltk_rem_file_output"
  and tk_mainloop : unit -> unit
      	= 1 "camltk_tk_mainloop"
  and internal_add_timer : int -> int -> TkTimer
      	= 2 "camltk_add_timer"
  and internal_rem_timer : TkTimer -> unit
        = 1 "camltk_rem_timer"
;;

value internal_tracevar : string -> int -> unit
      	= 2 "camltk_trace_var"
and   internal_tracevis : string -> string -> unit
      	= 2 "camltk_wait_vis"
and   internal_tracedestroy : string -> string -> unit
      	= 2 "camltk_wait_des"
;;
