type location =
    Loc of int     (* Position of the first character *)
         * int     (* Position of the next character following the last one *)
;;

value get_current_location : unit -> location
  and no_location : location
  and prerr_location : location -> unit
  and prerr_input_name : unit -> unit
  and input_name : string ref
  and input_chan : in_channel ref
  and input_lexbuf : lexing__lexbuf ref
;;
