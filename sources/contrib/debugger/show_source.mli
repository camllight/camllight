(* Print the line containing the point *)
value show_point : string -> int -> bool -> bool -> unit;;

(* Tell Emacs we are nowhere in the source. *)
value show_no_point : unit -> unit;;

(* Display part of the source. *)
value show_listing : string -> int -> int -> int -> bool -> unit;;
