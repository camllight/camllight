(***************************** Tools for Unix ********************************)

#open "unix";;

(* Convert a socket name into a socket address. *)
value convert_adress : string -> socket_domain * sockaddr;;

(* Report an unix error. *)
value report_error : exn -> unit;;
