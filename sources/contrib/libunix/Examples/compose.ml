#open "sys";;
#open "unix";;

let main () =
  let n = vect_length command_line - 1 in
  for i = 1 to n - 1 do
    let (fd_out, fd_in) = pipe() in
    match fork() with
      0 ->
        dup2 fd_in stdout;
        close fd_in;
        close fd_out;
        execv "/bin/sh" [| "/bin/sh"; "-c"; command_line.(i) |]
    | _ ->
        dup2 fd_out stdin;
        close fd_in;
        close fd_out
  done;
  match fork() with
    0 ->
      execv "/bin/sh" [|"/bin/sh"; "-c"; command_line.(n) |]
  | _ ->
      let rec wait_for_children retcode =
        try
          match wait() with
            (pid, WEXITED n) -> wait_for_children (retcode lor n)
          | (pid, _)         -> wait_for_children 127
        with
            Unix_error(ECHILD, _, _) -> retcode in
      exit (wait_for_children 0)
;;
handle_unix_error main ();;
