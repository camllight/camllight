(* Lower level interface *)

#open "unix";;
#open "support";;

exception TkError of string
      (* Raised by the communication functions *)
;;

value debug : bool ref 
      (* When set to true, displays intermediate Tcl code *)
;;

type write_buffer
      (* Buffer for sending Tcl code *)
and result_buffer
      (* Buffer for reading Tcl results *)
and callback_buffer
      (* Buffer for reading callback arguments *)
;;

value Send2TkStart : bool -> write_buffer
and   Send2Tk : write_buffer -> string -> unit
and   Send2TkEval : write_buffer -> result_buffer
      (* Protocol for evaluating Tcl code *)
;;

value arg_GetTkToken : callback_buffer -> string
and arg_GetTkString : callback_buffer -> string
and arg_GetTkTokenList : callback_buffer -> string list
      (* Extracting arguments from callback information *)
;;

value res_GetTkToken : result_buffer -> string
and   res_GetTkString : result_buffer -> string
and   res_GetTkTokenList: result_buffer -> string list
      (* Extracting results of function call *)
;;

value register_callback : Widget -> (callback_buffer -> unit) -> string
      (* Callback support *)
;;

value result_header : string
and   result_string_header: string
and   result_footer : string
      (* Internal *)
;;

value OpenTk : unit -> Widget
and   OpenTkClass: string -> Widget
and   CloseTk : unit -> unit
and   MainLoop : unit -> unit
and   add_fileinput : file_descr -> (unit -> unit) -> unit
and   remove_fileinput: file_descr -> unit
      (* see [tk] module *)
;;
