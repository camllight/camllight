(* Lower level interface *)

#open "unix";;
#open "support";;

exception TkError of string
      (* Raised by the communication functions *)
;;

value debug : bool ref 
      (* When set to true, displays approximation of intermediate Tcl code *)
;;

type callback_buffer
      (* Buffer for reading callback arguments *)
and result_buffer
      (* Buffer for reading Tcl results *)
;;

type TkArgs =
    TkToken of string
  | TkTokenList of TkArgs list		(* to be expanded *)
  | TkQuote of TkArgs			(* mapped to Tcl list *)
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

value remove_callbacks: Widget -> unit
      (* Clean up callbacks associated to widget. Must be used only when
      	 the Destroy event is bind by the user and masks the default
	 Destroy event binding *)
;;



value OpenTk : unit -> Widget
and   OpenTkClass: string -> Widget
and   OpenTkDisplayClass: string -> string -> Widget
and   CloseTk : unit -> unit
and   MainLoop : unit -> unit
and   add_fileinput : file_descr -> (unit -> unit) -> unit
and   remove_fileinput: file_descr -> unit
      (* see [tk] module *)
and   TkEval : TkArgs vect -> result_buffer
;;
