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
;;

type TkArgs =
    TkToken of string
  | TkTokenList of TkArgs list		(* to be expanded *)
  | TkQuote of TkArgs			(* mapped to Tcl list *)
;;


value splitlist : string -> string list
      	= 1 "camltk_splitlist"
;;

value arg_GetTkToken : callback_buffer -> string
      (* Extracting arguments from callback information *)
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
and   TkEval : TkArgs vect -> string
;;
