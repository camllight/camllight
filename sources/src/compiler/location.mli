type location =
    Loc of int     (* Position of the first character *)
         * int     (* Position of the next character following the last one *)
;;

value get_current_location : unit -> location
  and no_location : location
  and input_name : string ref
  and input_chan : in_channel ref
  and input_lexbuf : lexing__lexbuf ref
;;

value output_location: out_channel -> location -> unit
and output_input_name: out_channel -> unit
;;
