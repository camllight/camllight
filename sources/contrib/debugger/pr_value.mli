#open "globals";;
#open "value";;

value print_value : VALUE -> typ -> unit
  and max_printer_depth : int ref
  and max_printer_steps : int ref
  and more : int -> unit
  and clear_ellipses : unit -> unit
;;

