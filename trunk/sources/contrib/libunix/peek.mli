(* To fetch various objects in C format from a string. *)

(* Arguments: a string and an offset into that string.
   Result: as follows. *)

value short : string -> int -> int = 2 "peek_short"
        (* A signed short *)
  and ushort : string -> int -> int = 2 "peek_ushort"
        (* An unsigned short *)
  and int : string -> int -> int = 2 "peek_int"
        (* A signed int *)
  and uint : string -> int -> int = 2 "peek_uint"
        (* An unsigned int *)
  and long : string -> int -> int = 2 "peek_long"
        (* A signed long (currenty truncated to 31 bits) *)
  and ulong : string -> int -> int = 2 "peek_ulong"
        (* An unsigned long (currently trucated to 31 bits) *)
  and nshort : string -> int -> int = 2 "peek_nshort"
        (* An unsigned short in network byte order *)
  and nlong : string -> int -> int = 2 "peek_nlong"
        (* An unsigned long in network byte order *)
  and cstring : string -> int -> string = 2 "peek_cstring"
        (* A null-terminated string *)
;;
