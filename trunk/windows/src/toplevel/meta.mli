(* To control the runtime system and bytecode interpreter *)

#open "obj";;

value global_data : obj vect
  and realloc_global_data : int -> unit = 1 "realloc_global"
  and static_alloc : int -> string = 1 "static_alloc"
  and static_free : string -> unit = 1 "static_free"
  and static_resize : string -> int -> string = 2 "static_resize"
  and interprete : string -> int -> int -> obj = 3 "start_interp"
  and available_primitives : unit -> string vect = 1 "available_primitives"
;;
