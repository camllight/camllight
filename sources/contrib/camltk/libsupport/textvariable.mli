type TextVariable
;;

value new : unit -> TextVariable
and   set : TextVariable -> string -> unit
and   get : TextVariable -> string
;;

value CAMLtoTKTextVariable : TextVariable -> string
;;
