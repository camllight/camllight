(* Support for Tk -textvariable option *)
#open "protocol";;
#open "support";;

type textVariable
      (* TextVariable is an abstract type *)
;;

value create : unit -> textVariable
      (* Allocation of a TextVariable *)
and   create_temporary : widget -> textVariable
      (* Allocation of a textVariable with lifetime associated to widget *)
and   set : textVariable -> string -> unit
      (* Setting the value of a TextVariable *)
and   get : textVariable -> string
      (* Reading the value of a TextVariable *)
and   name : textVariable -> string
      (* Its tcl name *)
;;

value cCAMLtoTKtextVariable : textVariable -> tkArgs
      (* Internal conversion function *)
;;
