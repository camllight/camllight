type status =
    Finished
  | Pause of unit -> status
;;

type graphic_context;;

value assign: graphic_context -> int -> int -> unit
  and exchange: graphic_context -> int -> int -> unit
  and display: 
    ((graphic_context -> status) * string * int * int * int * int) vect -> 
    int -> int -> unit
;;
