(* Internals of the Unix library. Not for direct use. *)

#open "unix";;

value install_signal_handler : (int -> unit) -> unit
        = 1 "unix_install_signal_handler"
  and set_signal : signal -> int -> int
        = 2 "unix_set_signal"
;;
