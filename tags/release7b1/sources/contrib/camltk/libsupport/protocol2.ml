(* Protocol with two separate process *)
#open "unix";;
#open "support";;

let debug = 
 ref (try sys__getenv "CAMLTKDEBUG"; true
      with Not_found -> false)
;;

(* Communication channels *)
let PipeCaml2Tk = ref stdout		(* commands to wish *)
and PipeTkCallB = ref stdout		(* callback information *)
and PipeTkResult = ref stdout		(* function call results *)
;;


(***************************************************************************)
(* Evaluating Tcl code                                                     *)
(***************************************************************************)

(* Catching tcl evaluation errors *)
let safeeval_code = "
proc safeeval { chan cmd } {
  if [catch { uplevel $cmd } errmsg] {
      puts stderr \"TkError $errmsg\";
      puts $chan \"TkError $errmsg\";
      flush $chan;
     } else {
  }
}
"
;;

(* Writing is not completely robust: if wish dies between two writes, we *) 
(* will probably get a SIGPIPE *)

type write_buffer == unit
;;

(* Start a Tcl phrase *)
(* flag is true if there will be a result *)
(*  (meaning report error on PipeTkResult instead of PipeTkCallB) *)
let Send2TkStart res  =
  let chan = if res then "$PipeTkResult" else "$PipeTkCallB" in
  if !debug then begin
      prerr_string "safeeval ";
      prerr_string chan;
      prerr_string " { "; 
      flush std_err
      end;
  write !PipeCaml2Tk "safeeval " 0 9;
  write !PipeCaml2Tk chan 0 (string_length chan);
  write !PipeCaml2Tk " { " 0 3;
  ()
;; 

(* Pieces of phrase. Space tokenisation here to avoid complex source *)
let Send2Tk _ s = 
  if !debug then begin
      prerr_string s; prerr_string " "; flush std_err
      end;
  write !PipeCaml2Tk s 0 (string_length s);
  write !PipeCaml2Tk " " 0 1;
  () 
;;

(* End phrase and eval *)
let Send2TkEval _ = 
  if !debug then begin
      prerr_string " }\n"; flush std_err
      end;
  write !PipeCaml2Tk " }\n" 0 3;
  ()
;;

(* Special send without  additional space, because of some mystery in the way 
   wish parses its input *)
let rawSend2Tk s = 
  if !debug then begin
      prerr_string s; flush std_err
      end;
  write !PipeCaml2Tk s 0 (string_length s);
  () 
;;


(***************************************************************************)
(* Reading Tcl arguments to callback or result of function call            *)
(***************************************************************************)
(* Read on port until end_char predicate is matched *)
let read_string end_char port = 
  let rec read_rec buf bufsize offs =
    let n = read port buf offs 1 in
      if n == 0 then (* eof *)
      	raise (TkError "terminated")
      else let c = nth_char buf offs in
      	if end_char c then sub_string buf 0 offs
        else let offs = succ offs in
    	        if offs = bufsize
		then read_rec (buf ^ create_string 32) (bufsize + 32) offs
		else read_rec buf bufsize offs in
  read_rec (create_string 32) 32 0
;;

(* Predicates *)
let token_sep = function
   ` ` -> true
 | `\n` -> true
 | _ -> false
;;

let newline c = c = `\n`
;;

let read_token port =
  let str = read_string token_sep port in
  if !debug then begin
     prerr_string "Received: ";
     prerr_string str;
     prerr_string "\n"
     end;
  if str = "TkError"
  then let errmsg = read_string newline port in 
       raise (TkError errmsg)
  else str
;;

(* Assumes the token is on a separate line *)
let read_token_line port =
  let str = read_string (function c -> c == `\n`) port in
  if !debug then begin
     prerr_string "Received: ";
     prerr_string str;
     prerr_string "\n"
     end;
  if str = "TkError"
  then let errmsg = read_string newline port in 
       raise (TkError errmsg)
  else str
;;

let read_line port = 
  let str = read_string newline port in
  if !debug then begin
     prerr_string "Received: ";
     prerr_string str;
     prerr_string "\n"
     end;
  if string_length str >= 7 & sub_string str 0 7 = "TkError"  
  then let errmsg = sub_string str 8 (string_length str - 8) in
       raise (TkError errmsg)
  else str
;;

(* Tokens may be received on the same line or on different lines *)

(* Mode 1: read token up to whitespace or \n *)
let GetTkToken = read_token
;;

(* Mode 2: read whitespace separated token list,\n terminated *)
let rec GetTkTokenList port =
  split_str (fun c -> c = ` `) (read_line port)
;;

(* Mode 3 : read string of given length *)
let GetTkString port =
  let count = int_of_string (read_line port) in
  let buf = create_string count in
  let rec read_rec offs size =
    let n = read port buf offs size in
    if n = size then buf
    else read_rec (offs+n) (size-n) 
  in
    let res = read_rec 0 count in
      read port " " 0 1; (* read the last \n *)
      res
;;

type callback_buffer == unit
;;

(* Extracting callback arguments from the pipe *)
(* we know that arguments are one by line      *)
(* if we use read_token, space-only tokens are dumped *)
(* i.e. bindings callbacks with %A *)
let arg_GetTkToken () = read_token_line !PipeTkCallB
and arg_GetTkTokenList ()  = GetTkTokenList !PipeTkCallB
and arg_GetTkString () = GetTkString !PipeTkCallB
;;

type result_buffer == unit
;;

(* Extracting results of function call *)
let res_GetTkToken () = GetTkToken !PipeTkResult
and res_GetTkTokenList ()  = GetTkTokenList !PipeTkResult
and res_GetTkString () = GetTkString !PipeTkResult
;;


(* Callback encoding *)
(* this is bogus if arg of callback is a multi-line string *)
let caml_callback = "
proc camlcb {cbid args} {
  global PipeTkCallB;
  puts $PipeTkCallB $cbid;
  foreach i $args {
   puts $PipeTkCallB \"$i\"
   };
  flush $PipeTkCallB;
  }
"
;;

(* Other wrapping stuff *)
let result_string_header = "nputs $PipeTkResult ["
and result_header = "puts $PipeTkResult ["
and result_footer = "]; flush $PipeTkResult;"
;;



(* Process stuff *)
let child_pid = ref 0
;;

let OpenTkInternal = function argv ->
  let dollardollar = string_of_int (getpid()) in
  (* create those pipes *)
    mkfifo ("/tmp/PipeCaml2Tk"^dollardollar) 0o600;
    mkfifo ("/tmp/PipeTkResult"^dollardollar) 0o600;
    mkfifo ("/tmp/PipeTkCallB"^dollardollar) 0o600;
  (* fork our slave Tk interpreter *)
    match fork() with
      0 -> (* the child *)
      	(* connect stdin to emission pipe *)
	  let i = open ("/tmp/PipeCaml2Tk"^dollardollar) [O_RDONLY] 0 in
	    dup2 i stdin; close i;
      	    execvp "wish" argv;	
	    raise (TkError "Can't start wish")
    | n -> (* Caml process *)
        child_pid := n;
      	(* open the other pipes *)
        PipeCaml2Tk := open ("/tmp/PipeCaml2Tk"^dollardollar) [O_WRONLY] 0;
      	(* initialize Tk  (unprotected calls) *)
      	rawSend2Tk("set PipeTkCallB [open /tmp/PipeTkCallB"^(dollardollar)^" w]\n");
      	rawSend2Tk("set PipeTkResult [open /tmp/PipeTkResult"^(dollardollar)^" w]\n");
        rawSend2Tk("proc nputs { pip str } { puts $pip [string length $str]; puts $pip $str}\n");
	rawSend2Tk safeeval_code;
	rawSend2Tk caml_callback;
      	(* open the other pipes *)
        PipeTkCallB := open ("/tmp/PipeTkCallB"^dollardollar) [O_RDONLY] 0;
      	PipeTkResult := open ("/tmp/PipeTkResult"^dollardollar) [O_RDONLY] 0;
	(* return the toplevel widget *)
        default_toplevel_widget
;;

(* This does not work 
(* If wish was killed, write raise SIGPIPE *)
(* Can't figure better way to check for child termination *)
(* the second waitpid() fails iff the child is dead ...      *)
  begin try
    waitpid [WNOHANG] !child_pid;
    waitpid [WNOHANG] !child_pid;
    (* Don't use Send2Tk because extra space prevents wish from exiting *)
    rawSend2Tk "exit\n"
  with _ -> ()
  end;
*)

let CloseTkInternal = function () ->
  begin try
    kill !child_pid SIGINT
  with _ -> ()
  end;
  (* cleanup *)
  close !PipeTkCallB;
  close !PipeCaml2Tk;
  close !PipeTkResult;
  let dollardollar = string_of_int (getpid()) in
  unlink ("/tmp/PipeCaml2Tk"^dollardollar);
  unlink ("/tmp/PipeTkCallB"^dollardollar);
  unlink ("/tmp/PipeTkResult"^dollardollar)
;;

(***************************************************************************)
(* Callbacks                                                               *)
(***************************************************************************)

let callback_table = 
   (hashtbl__new 73 : (string, callback_buffer -> unit) hashtbl__t) 
;;

let new_function_id =
  let counter = ref 0 in
  function () ->
    incr counter;
    "f" ^ (string_of_int !counter)
;;


let register_callback f =
  let id = new_function_id () in
    hashtbl__add callback_table id f;
    id
;;

let NextCallback () =
  let str = GetTkToken !PipeTkCallB in
    try (hashtbl__find callback_table str) () 
    with exc -> CloseTkInternal(); raise exc
;;

(***************************************************************************)
(* File descriptor callbacks                                               *)
(***************************************************************************)
let channel_callbacks = 
  (hashtbl__new 11 : (file_descr, (unit -> unit)) hashtbl__t);;
let channels  = ref ([] : file_descr list)
;;

let add_fileinput fd cb =
  channels := fd :: !channels;
  hashtbl__add channel_callbacks fd cb
;;

let remove_fileinput fd = 
  channels := except fd !channels;
  hashtbl__remove channel_callbacks fd
;;


let OpenTk () = 
  let top = OpenTkInternal [| "wish" |] in
    add_fileinput !PipeTkCallB NextCallback;
    (top : Widget)
;;
let OpenTkClass s = 
  let top = OpenTkInternal  [| "wish"; "-name"; s |] in
    add_fileinput !PipeTkCallB NextCallback;
    (top : Widget)
;;

let CloseTk () = 
  remove_fileinput !PipeTkCallB; CloseTkInternal()
;;

let NextEvent () =
    let (ins,_,_) = select !channels [] [] (-1.0) in
       do_list (fun fd -> (hashtbl__find channel_callbacks fd) ())
               ins
;;

let MainLoop () =
  while true do NextEvent() done
;;
