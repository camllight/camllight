(*********************** Loading management ***********************)
#open "unix";;
#open "unix_tools";;
#open "debugger_config";;
#open "misc";;
#open "const";;
#open "lambda";;
#open "primitives";;
#open "constants";;
#open "parameters";;
#open "symtable";;
#open "input_handling";;
#open "communication";;
#open "program_loading";;
#open "modules2";;
#open "events";;
#open "time_travel";;

(*** Load debugging information. ***)

let load_information file =
  let inchan =
    try
      open_in_bin file
    with
      sys__Sys_error s ->
      	prerr_endline ("Can't open " ^ file ^ " : " ^ s);
	raise Toplevel
  in
    (let pos = in_channel_length inchan - Trailer_size in
       if pos < 0 then
         (prerr_endline ("The file " ^ file ^ " is not a bytecode executable file.");
          close_in inchan;
          raise Toplevel);
       seek_in inchan pos);
    skip_binary_int inchan;
    skip_binary_int inchan;
    let symbol_size = input_binary_int inchan in
      let debug_size = input_binary_int inchan in
      	if input_binary_int inchan <> Exec_magic then
	  (prerr_endline ("The file " ^ file ^ " is not a bytecode executable file.");
	   close_in inchan;
	   raise Toplevel)
        else if (symbol_size = 0) || (debug_size = 0) then
	  (prerr_endline
      	     ("The file " ^ file ^ " does not contain enough debugging information.");
           prerr_endline "Please recompile it with the '-g' option.";
	   close_in inchan;
           raise Toplevel);
      	seek_in
      	  inchan
          (in_channel_length inchan - Trailer_size - symbol_size - debug_size);
      	load_linker_tables inchan;
      	load_events inchan;
      	close_in inchan;;

(*** Connection opening and control. ***)

(* Name of the file if the socket is in the unix domain.*)
let file_name = ref (None : string option);;

(* Default connection handler. *)
let buffer = create_string 1024;;
let control_connection pid fd =
  if (read fd.Io_fd buffer 0 1024) = 0 then
    forget_process fd pid
  else
    (prerr_string "Garbage data from process ";
     prerr_int pid;
     prerr_endline "");;

(* Accept a connection from another process. *)
let accept_connection continue fd =
  let (sock, _) = accept fd.Io_fd in
    let io_chan = io_channel_of_descr sock in
      let pid = input_binary_int io_chan.Io_in in
        if pid = 0 then
          let pid' = input_binary_int io_chan.Io_in in
      	     new_checkpoint pid' io_chan;
             input_handling__add_file io_chan (control_connection pid');
	     output_binary_int
      	       io_chan.Io_out
      	       (if !enable_sigint then 1 else 0);
      	     continue ()
        else
          if set_file_descriptor pid io_chan then
      	    input_handling__add_file io_chan (control_connection pid);;

(* Initialize the socket. *)
let open_connection address continue =
  try
    let (sock_domain, sock_address) = convert_adress address in
      file_name :=
        (match sock_address with
      	   ADDR_UNIX file ->
	     Some file
         | _ ->
       	     None);
      let sock = socket sock_domain SOCK_STREAM 0 in
      	(try
           bind sock sock_address;
           listen sock 3;
	   connection := io_channel_of_descr sock;
           input_handling__add_file !connection (accept_connection continue);
	   connection_opened := true
	 with x -> close sock; raise x)
  with
    Failure _ -> raise Toplevel
  | (Unix_error _) as err -> report_error err; raise Toplevel;;

(* Close the socket. *)
let close_connection () =
  if !connection_opened then
    (connection_opened := false;
     input_handling__remove_file !connection;
     close_io !connection;
     match !file_name with
       Some file ->
         unlink file
     | None ->
         ());;

(*** Kill program. ***)
let loaded = ref false;;

let kill_program () =
  loaded := false;
  close_connection ();
  kill_all_checkpoints ();
  close_all_modules ();
  history__empty_history ();;

let ask_kill_program () =
  if not !loaded then
    true
  else
    let answer = yes_or_no "A program is being debugged already. Kill it" in
      if answer then
        kill_program ();
      answer;;

(*** Program loading and initializations. ***)

let install_events () =
  if !debug_loading then
    (print_string "Installing events..."; print_newline ());
  do_list (function {ev_pos = pos} -> set_event pos) !events;;
                                      (* Not optimized : *)
                                      (* the same position may be installed *)
                                      (* several times !!! *)
let mark_globals () =
  if !debug_loading then
    (print_string "Marking global variables..."; print_newline ());
  hashtbl__do_table
    (fun qualid slot ->
      if qualid.qual <> "sys" then mark_global_uninitialized slot)
    (!global_table).num_tbl;;

let initialize_loading () =
  if !debug_loading then
    (print_string "Loading debugging informations..."; print_newline ());
  load_information
    (try search_in_path !program_name with
       Not_found ->
      	 prerr_endline "Program not found."; raise Toplevel);
  if !debug_loading then
    (print_string "Opening a socket..."; print_newline ());
  open_connection
    !socket_name
    (function () -> go_to 0;
                    install_events ();
                    mark_globals ();
                    exit_main_loop ());;

(* Ensure the program is already loaded. *)
let ensure_loaded () =
  if not !loaded then
    (print_string "Loading program..."; flush std_out;
     if !program_name = "" then
       (prerr_endline "No program specified."; raise Toplevel);
     try
       initialize_loading ();
       initialize_modules
      	 ((intersect default_modules !modules) @ always_opened_modules);
       !launching_function ();
       if !debug_loading then
         (print_string "Waiting for connection..."; print_newline ());
       main_loop ();
       loaded := true;
       print_string "done."; print_newline ()
     with
       x -> kill_program (); raise x);;
