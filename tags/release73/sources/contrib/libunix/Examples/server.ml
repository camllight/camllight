#open "sys";;
#open "unix";;

let main () =
  let port_number = int_of_string command_line.(1)
  and args = sub_vect command_line 2 (vect_length command_line - 2) in
  let sock =
    socket PF_INET SOCK_STREAM 0 in
  let my_host_addr =
    (gethostbyname(gethostname())).h_addr_list.(0) in
  bind sock (ADDR_INET(my_host_addr, port_number));
  listen sock 3;
  while true do
    let (s, calleraddr) = accept sock in
      begin match calleraddr with
        ADDR_INET(host,_) ->
          print_string ("Connection from " ^ string_of_inet_addr host);
          print_newline()
      | ADDR_UNIX _ ->
          print_string "Connection from the Unix domain (???)";
          print_newline()
      end;
      match fork() with
        0 -> if fork() <> 0 then exit 0;
             dup2 s stdin;
             dup2 s stdout;
             dup2 s stderr;
             close s;
             execvp args.(0) args
      | _ -> wait();
             close s
  done
;;
handle_unix_error main ();;
