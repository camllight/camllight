#open "unix";;
#open "support";;

exception TkError of string
;;

value debug : bool ref ;;

type write_buffer
and result_buffer
and callback_buffer
;;

value Send2TkStart : bool -> write_buffer
and   Send2Tk : write_buffer -> string -> unit
and   Send2TkEval : write_buffer -> result_buffer
;;

(* Extracting arguments from callback information *)
value arg_GetTkToken : callback_buffer -> string
and arg_GetTkString : callback_buffer -> string
and arg_GetTkTokenList : callback_buffer -> string list
;;

(* Extracting results of function call *)
value res_GetTkToken : result_buffer -> string
and   res_GetTkString : result_buffer -> string
and   res_GetTkTokenList: result_buffer -> string list
;;

value register_callback : (callback_buffer -> unit) -> string;;

(* Other stuff *)
value result_header : string
and   result_string_header: string
and   result_footer : string
;;

value OpenTk : unit -> Widget
and   OpenTkClass: string -> Widget
and   CloseTk : unit -> unit
and   MainLoop : unit -> unit
and   add_fileinput : file_descr -> (unit -> unit) -> unit
and   remove_fileinput: file_descr -> unit
;;
