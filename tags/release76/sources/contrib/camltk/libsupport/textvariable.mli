(* Support for Tk -textvariable option *)
#open "protocol";;

type TextVariable
      (* TextVariable is an abstract type *)
;;

value new : unit -> TextVariable
      (* Allocation of a TextVariable *)
and   set : TextVariable -> string -> unit
      (* Setting the value of a TextVariable *)
and   get : TextVariable -> string
      (* Reading the value of a TextVariable *)
and   name : TextVariable -> string
      (* Its tcl name *)
;;

value CAMLtoTKTextVariable : TextVariable -> TkArgs
      (* Internal conversion function *)
;;
