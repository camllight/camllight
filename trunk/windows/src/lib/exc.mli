(* Exceptions *)

value raise : exn -> 'a = 1 "raise";;
        (* Raise the given exception value. *)

(*** A few general-purpose predefined exceptions. *)

exception Out_of_memory;;
        (* Raised by the garbage collector, when there is insufficient
           memory to complete the computation. *)
exception Invalid_argument of string;;
        (* Raised by some library functions, to signal that the given
           arguments do not make sense. *)
exception Failure of string;;
        (* Raised by some library functions, to signal that they are
           undefined on the given arguments. *)
exception Not_found;;
        (* Raised by search library functions, when the required object
           could not be found. *)
exception Exit;;
        (* This exception is not raised by any library function.  It is
	   provided for use in your programs. *)

value failwith : string -> 'a;;
        (* Raise exception [Failure] with the given string. *)
value invalid_arg : string -> 'a;;
        (* Raise exception [Invalid_argument] with the given string. *)


