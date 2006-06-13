(* Standard I/O channels *)

type file_descr == int;;

let stdin = 0
and stdout = 1
and stderr = 2
;;

(* Error reporting *)

let handle_unix_error f arg =
  try
    f arg
  with Unix_error(err, fun_name, arg) ->
    prerr_string sys__command_line.(0);
    prerr_string ": \"";
    prerr_string fun_name;
    prerr_string "\" failed";
    if string_length arg > 0 then begin
      prerr_string " on \"";
      prerr_string arg;
      prerr_string "\""
    end;
    prerr_string ": ";
    prerr_endline (error_message err);
    io__exit 2
;;

(* Signals *)

let default_handler () = ();;

let handler_table = [|
  default_handler; default_handler; default_handler; default_handler;
  default_handler; default_handler; default_handler; default_handler;
  default_handler; default_handler; default_handler; default_handler;
  default_handler; default_handler; default_handler; default_handler;
  default_handler; default_handler; default_handler; default_handler;
  default_handler; default_handler; default_handler; default_handler;
  default_handler; default_handler; default_handler; default_handler;
  default_handler; default_handler; default_handler; default_handler
|];;

let signal_handler n =
  if n < 0 || n >= 32 then () else handler_table.(n)()
;;  

let signal sig = function
    Signal_default ->
      let n = iunix__set_signal sig 0 in handler_table.(n) <- default_handler
  | Signal_ignore ->
      let n = iunix__set_signal sig 1 in handler_table.(n) <- default_handler
  | Signal_handle f ->
      iunix__install_signal_handler signal_handler;
      let n = iunix__set_signal sig 2 in handler_table.(n) <- f
;;

(* High-level process management (system, popen) *)

let system cmd =
  match fork() with
     0 -> execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]; exit 127
  | id -> snd(waitpid [] id)
;;

type popen_process =
    Process of in_channel * out_channel
  | Process_in of in_channel
  | Process_out of out_channel
;;

let popen_processes = (hashtbl__new 7 : (popen_process, int) hashtbl__t);;

let open_proc cmd proc input output toclose =
  match fork() with
     0 -> if input != stdin then begin dup2 input stdin; close input end;
          if output != stdout then begin dup2 output stdout; close output end;
          do_list close toclose;
          execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |];
          exit 127
  | id -> hashtbl__add popen_processes proc id
;;

let open_process_in cmd =
  let (in_read, in_write) = pipe() in
  let inchan = in_channel_of_descr in_read in
  open_proc cmd (Process_in inchan) stdin in_write [in_read]; 
  close in_write;
  inchan
;;

let open_process_out cmd =
  let (out_read, out_write) = pipe() in
  let outchan = out_channel_of_descr out_write in
  open_proc cmd (Process_out outchan) out_read stdout [out_write];
  close out_read;
  outchan
;;

let open_process cmd =
  let (in_read, in_write) = pipe() in
  let (out_read, out_write) = pipe() in
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  open_proc cmd (Process(inchan, outchan))
            out_read in_write [out_write; in_read];
  (inchan, outchan)
;;

let find_proc_id fun_name proc =
  try
    let pid = hashtbl__find popen_processes proc in
    hashtbl__remove popen_processes proc;
    pid
  with Not_found ->
    raise(Unix_error(EBADF, fun_name, ""))
;;
let close_process_in inchan =
  let pid = find_proc_id "close_process_in" (Process_in inchan) in
  close_in inchan;
  snd(waitpid [] pid)
;;
let close_process_out outchan =
  let pid = find_proc_id "close_process_out" (Process_out outchan) in
  close_out outchan;
  snd(waitpid [] pid)
;;
let close_process (inchan, outchan) =
  let pid = find_proc_id "close_process" (Process(inchan, outchan)) in
  close_in inchan; close_out outchan;
  snd(waitpid [] pid)
;;

(* High-level network functions *)

let open_connection sockaddr =
  let domain =
    match sockaddr with ADDR_UNIX _ -> PF_UNIX | ADDR_INET(_,_) -> PF_INET in
  let sock =
    socket domain SOCK_STREAM 0 in
  connect sock sockaddr;
  (in_channel_of_descr sock, out_channel_of_descr sock)
;;

let shutdown_connection inchan =
  shutdown (descr_of_in_channel inchan) SHUTDOWN_SEND
;;

let establish_server server_fun sockaddr =
  let domain =
    match sockaddr with ADDR_UNIX _ -> PF_UNIX | ADDR_INET(_,_) -> PF_INET in
  let sock =
    socket domain SOCK_STREAM 0 in
  bind sock sockaddr;
  listen sock 3;
  while true do
    let (s, caller) = accept sock in
    (* The "double fork" trick, the process which calls server_fun will not
       leave a zombie process *)
    match fork() with
       0 -> if fork() != 0 then exit 0; (* The son exits, the grandson works *)
            let inchan = in_channel_of_descr s in
            let outchan = out_channel_of_descr s in
            server_fun inchan outchan;
            close_in inchan;
            close_out outchan
    | id -> close s; let _ = waitpid [] id (* Reclaim the son *) in ()
  done
;;


