#open "globals";;

value reset_type_var_name : unit -> unit
  and name_of_type_var : 'a -> string
;;

value print_constr : 'a global -> unit
  and print_type_constr : 'a global -> unit
  and print_value : 'a global -> unit
  and print_label : 'a global -> unit
;;

value prerr_constr : 'a global -> unit
  and prerr_type_constr : 'a global -> unit
  and prerr_value : 'a global -> unit
  and prerr_label : 'a global -> unit
;;

value print_type : typ -> unit
  and print_one_type : typ -> unit
;;

value prerr_type : typ -> unit
  and prerr_one_type : typ -> unit
;;
