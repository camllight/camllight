(* Synthetic events *)

value send : string -> support__widget -> unit;;
value broadcast : string -> unit;;
value bind : support__widget -> string -> (support__widget -> unit) -> unit;;
value remove : support__widget -> string -> unit;;
