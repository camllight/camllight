#open "unix";;

let split_words s =
  let rec skip_blanks i =
    if i < string_length s && nth_char s i = ` `
    then skip_blanks (i+1)
    else i in
  let rec split start i =
    if i >= string_length s then
      [sub_string s start (i-start)]
    else if nth_char s i = ` ` then
      let j = skip_blanks i in
      sub_string s start (i-start) :: split j j
    else
      split start (i+1) in
  vect_of_list (split 0 0);;

let main () =
  try
    while true do
      let cmd = input_line std_in in
      let words = split_words cmd in
      match fork() with
        0 ->
          begin try
            execvp words.(0) words
          with Unix_error(err, _, _) ->
            print_string
              ("Cannot execute " ^ words.(0) ^ " : " ^ error_message err);
            print_newline();
            exit 255
          end
      | pid_son ->
          match wait() with
            (pid, WEXITED 255) ->
              ()
          | (pid, WEXITED status) ->
              print_string "Program exited with code ";
              print_int status;
              print_newline()
          | (pid, WSIGNALED(sig, coredumped)) ->
              print_string "Program killed by signal ";
              print_int sig;
              print_newline();
              if coredumped then begin
                print_string "Yo mama! Core dumped";
                print_newline()
              end
          | (pid, WSTOPPED sig) ->
              print_string "Program stopped (???)";
              print_newline()
    done
  with End_of_file ->
    ()
;;
handle_unix_error main ();;
