(* To store various objects in a string, in C format. *)

(* Arguments: the destination string, the offset into that string, and
   the object to poke. Result: none. *)

value short : string -> int -> int -> unit = 3 "poke_short"
        (* A short *)
  and int : string -> int -> int -> unit = 3 "poke_int"
        (* An int *)
  and long : string -> int -> int -> unit = 3 "poke_long"
        (* A long *)
  and nshort : string -> int -> int -> unit = 3 "poke_nshort"
        (* An unsigned short in network byte order *)
  and nlong : string -> int -> int -> unit = 3 "poke_nlong"
        (* An unsigned long in network byte order *)
;;
