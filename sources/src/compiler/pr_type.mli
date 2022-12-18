#open "globals";;

value reset_type_var_name : unit -> unit
  and name_of_type_var : typ -> string
;;

value output_type : out_channel -> typ -> unit
  and output_one_type : out_channel -> typ -> unit
;;

value output_schema : out_channel -> typ -> unit
;;

value output_type_constr : out_channel -> type_desc global -> unit
  and output_value : out_channel -> value_desc global -> unit
  and output_constr : out_channel -> constr_desc global -> unit
  and output_label : out_channel -> label_desc global -> unit
;;
