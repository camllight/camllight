(* Support for Tk -textvariable option *)

type TextVariable
      (* TextVariable is an abstract type *)
;;

value new : unit -> TextVariable
      (* Allocation of a TextVariable *)
and   set : TextVariable -> string -> unit
      (* Setting the value of a TextVariable *)
and   get : TextVariable -> string
      (* Reading the value of a TextVariable *)
;;

value CAMLtoTKTextVariable : TextVariable -> string
      (* Internal conversion function *)
;;
