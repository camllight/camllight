(* Lower level interface *)

#open "unix";;
#open "support";;

exception TkError of string
      (* Raised by the communication functions *)
;;

value   cTKtoCAMLwidget : string -> widget
      (* Conversion functions *)
;;

value debug : bool ref 
      (* When set to true, displays approximation of intermediate Tcl code *)
;;

type callback_buffer == string list
      (* Buffer for reading callback arguments *)
;;

type tkArgs =
    TkToken of string
  | TkTokenList of tkArgs list		(* to be expanded *)
  | TkQuote of tkArgs			(* mapped to Tcl list *)
;;

type cbid
;;

value splitlist : string -> string list
      	= 1 "camltk_splitlist"
;;

value callback_naming_table : (cbid, callback_buffer -> unit) hashtblc__t
and   callback_memo_table : (widget, cbid) hashtblc__t
      (* Exported for debug purposes only. Don't use them unless you
         know what you are doing *)
;;
value new_function_id : unit -> cbid
and   string_of_cbid : cbid -> string
;;

value register_callback : widget -> (callback_buffer -> unit) -> string
      (* Callback support *)
and  clear_callback : cbid -> unit
      (* Remove a given callback from the table *)
and remove_callbacks: widget -> unit
      (* Clean up callbacks associated to widget. Must be used only when
      	 the Destroy event is bind by the user and masks the default
	 Destroy event binding *)
;;

value add_destroy_hook : (widget -> unit) -> unit
;;


(* Use TkEval instead *)
value tcl_direct_eval : tkArgs vect -> string
      	= 1 "camltk_tcl_direct_eval"
;;


value openTk : unit -> widget
and   openTkClass: string -> widget
and   openTkDisplayClass: string -> string -> widget
and   closeTk : unit -> unit
and   mainLoop : unit -> unit
and   add_fileinput : file_descr -> (unit -> unit) -> unit
and   remove_fileinput: file_descr -> unit
and   add_fileoutput : file_descr -> (unit -> unit) -> unit
and   remove_fileoutput: file_descr -> unit
      (* see [tk] module *)
and   tkEval : tkArgs vect -> string
and   tkDo : tkArgs vect -> unit
and   tkreturn : string -> unit
         = 1 "camltk_return"
;;

type timer
;;

value  add_timer : int -> (unit -> unit) -> timer
and    remove_timer : timer -> unit
;;

type tkEventFlags =
    TK_DONT_WAIT
  | TK_X_EVENTS
  | TK_FILE_EVENTS
  | TK_TIMER_EVENTS
  | TK_IDLE_EVENTS
  | TK_ALL_EVENTS
;;

value tk_dooneevent : tkEventFlags list -> int
      	= 1 "camltk_dooneevent"
;;

value var_handle : string -> (unit -> unit) -> unit
;;
