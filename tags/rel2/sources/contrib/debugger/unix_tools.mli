(***************************** Tools for Unix ********************************)

#open "unix";;

(* Convert a socket name into a socket address. *)
value convert_adress : string -> socket_domain * sockaddr;;

(* Report an unix error. *)
value report_error : exn -> unit;;

(* Find program `name' in `PATH'. *)
(* Return the full path if found. *)
(* Raise `Not_found' otherwise. *)
value search_in_path : string -> string;;

(* Path expansion. *)
value expand_path : string -> string;;
