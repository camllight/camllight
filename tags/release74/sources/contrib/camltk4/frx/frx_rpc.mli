(* Some notion of RPC *)

value register : string -> (string list -> unit) -> unit
  (* [register external_name f] *)
;;
value invoke : string -> string -> string list -> string
  (* [invoke interp name args] *)
;;
value async_invoke : string -> string -> string list -> unit
  (* [async_invoke interp name args] *)
;;
value rpc_info : string -> string
  (* [rpc_info interp] *)
;;
