(* Exceptions *)

let failwith s = raise (Failure s)
and invalid_arg s = raise (Invalid_argument s)
;;
