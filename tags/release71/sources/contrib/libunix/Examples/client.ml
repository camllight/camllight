#open "sys";;
#open "unix";;

exception Finished;;

let retransmit input output =
  let buffer = create_string 4096 in
  try
    while true do
      match read input buffer 0 4096 with
        0 -> raise Finished
      | n -> write output buffer 0 n
    done
  with Finished ->
    ()
;;

let main () =
  let server_name = command_line.(1)
  and port_number = int_of_string command_line.(2) in
  let server_addr =
    try
      inet_addr_of_string server_name
    with Failure _ ->
      try
        (gethostbyname server_name).h_addr_list.(0)
      with Not_found ->
        prerr_endline ("Unknown host " ^ server_name);
        exit 2 in
  let sock =
    socket PF_INET SOCK_STREAM 0 in
  connect sock (ADDR_INET(server_addr, port_number));
  match fork() with
    0 -> retransmit stdin sock;
         shutdown sock SHUTDOWN_SEND;
         exit 0
  | _ -> retransmit sock stdout;
         close stdout;
         wait()
;;
handle_unix_error main ();;
