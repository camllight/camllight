(* Protocol with two separate process *)
#open "unix";;
#open "support";;

let debug = 
 ref (try sys__getenv "CAMLTKDEBUG"; true
      with Not_found -> false)
;;

(* Communication channels *)
let PipeCaml2Tk = ref stdout
and PipeTkCallB = ref stdout
and PipeTkResult = ref stdout
;;


let Send2TkStart chan  =
  if !debug then begin
      prerr_string "safeeval ";
      prerr_string chan;
      prerr_string " { "; 
      flush std_err
      end;
  write !PipeCaml2Tk "safeeval " 0 9;
  write !PipeCaml2Tk chan 0 (string_length chan);
  write !PipeCaml2Tk " { " 0 3
;; 

(* use "safe_write" instead ? *)
let Send2Tk s = 
  if !debug then begin
      prerr_string s; prerr_string " "; flush std_err
      end;
  write !PipeCaml2Tk s 0 (string_length s);
  write !PipeCaml2Tk " " 0 1;
  () 
;;

let Send2TkEval () = 
  if !debug then begin
      prerr_string " }\n"; flush std_err
      end;
  write !PipeCaml2Tk " }\n" 0 3;
  ()
;;


(* Communication with Tk *)
exception TkError of string
;;

let read_string end_char port = 
  let rec read_rec buf bufsize offs =
    let n = read port buf offs 1 in
      if n == 0 then (* wish interpreter has quit *)
      	raise (TkError "terminated")
      else let c = nth_char buf offs in
      	if end_char c then sub_string buf 0 offs
        else let offs = succ offs in
    	        if offs = bufsize
		then read_rec (buf ^ create_string 32) (bufsize + 32) offs
		else read_rec buf bufsize offs in
  read_rec (create_string 32) 32 0
;;

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

(* split a string according to char_sep predicate *)
let split_str char_sep str =
  let len = string_length str in
  let rec skip_sep cur =
    if cur >= len then cur
    else if char_sep (nth_char str cur) then skip_sep (succ cur)
    else cur  in
  let rec split beg cur =
    if cur >= len then 
      if beg = cur then []
      else [sub_string str beg (len - beg)]
    else if char_sep (nth_char str cur) 
         then 
      	   let nextw = skip_sep cur in
      	    (sub_string str beg (cur - beg))
      	      ::(split nextw nextw)
	 else split beg (succ cur) in
  let wstart = skip_sep 0 in
  split wstart wstart
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


(* could use argument to do some setup *)
let OpenTkInternal = function argv ->
  (* rw_rw_rw for named pipes *)
  umask 0;
  let dollardollar = string_of_int (getpid()) in
  (* create those pipes *)
    mkfifo ("/tmp/PipeCaml2Tk"^dollardollar) 0o666;
    mkfifo ("/tmp/PipeTkResult"^dollardollar) 0o666;
    mkfifo ("/tmp/PipeTkCallB"^dollardollar) 0o666;
  (* open the shared one *)
    PipeCaml2Tk := open ("/tmp/PipeCaml2Tk"^dollardollar) [O_RDWR] 0;
  (* fork our slave Tk interpreter *)
    match fork() with
      0 -> (* the child *)
      	(* connect stdin to emission pipe *)
	  dup2 !PipeCaml2Tk stdin;
      	  execvp "wish" argv;	
	  failwith "exec failed"
    | _ -> (* Caml process *)
      	(* open the other pipes *)
        PipeTkCallB := open ("/tmp/PipeTkCallB"^dollardollar) [O_RDWR] 0;
      	PipeTkResult := open ("/tmp/PipeTkResult"^dollardollar) [O_RDWR] 0;
      	(* initialize Tk  (unprotected calls) *)
      	Send2Tk("set PipeCaml2Tk [open /tmp/PipeCaml2Tk"^(dollardollar)^" a+]\n");
      	Send2Tk("set PipeTkCallB [open /tmp/PipeTkCallB"^(dollardollar)^" a+]\n");
      	Send2Tk("set PipeTkResult [open /tmp/PipeTkResult"^(dollardollar)^" a+]\n");
        Send2Tk("proc nputs { pip str } { puts $pip [string length $str]; puts $pip $str}\n");
	Send2Tk safeeval_code;
	(* return the toplevel widget *)
        default_toplevel_widget
;;

let OpenTk () = OpenTkInternal [| "wish" |]
;;
let OpenTkClass s = OpenTkInternal [| "wish"; "-name"; s |]
;;


let CloseTk = function () ->
  (* Don't use Send2Tk because last space prevents wish from exiting *)
  write !PipeCaml2Tk "exit\n" 0 5;
  (* cleanup *)
  close !PipeTkCallB;
  close !PipeCaml2Tk;
  close !PipeTkResult;
  let dollardollar = string_of_int (getpid()) in
  unlink ("/tmp/PipeCaml2Tk"^dollardollar);
  unlink ("/tmp/PipeTkCallB"^dollardollar);
  unlink ("/tmp/PipeTkResult"^dollardollar)
;;




