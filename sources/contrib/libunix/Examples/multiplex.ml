#open "unix";;

let rec really_read fd buff start length =
  if length <= 0 then () else
    match read fd buff start length with
      0 -> raise End_of_file
    | n -> really_read fd buff (start+n) (length-n);;

let buffer = create_string 258;;

let multiplex channel inputs outputs =
  let input_fds = channel :: list_of_vect inputs in
  try
    while true do
      let (ready_fds, _, _) = select input_fds [] [] (-1.0) in
      for i = 0 to vect_length inputs - 1 do
        if mem inputs.(i) ready_fds then begin
          let n = read inputs.(i) buffer 2 255 in
          set_nth_char buffer 0 (char_of_int i);
          set_nth_char buffer 1 (char_of_int n);
          write channel buffer 0 (n+2);
          ()
        end
      done;
      if mem channel ready_fds then begin
        really_read channel buffer 0 2;
        let i = int_of_char(nth_char buffer 0)
        and n = int_of_char(nth_char buffer 1) in
        if n = 0 then
          close outputs.(i)
        else begin
          really_read channel buffer 0 n;
          write outputs.(i) buffer 0 n;
          ()
        end
      end
    done
  with End_of_file ->
    ()
;;

(* Le programme de test *)

let nprocs = 5;;

let seed = ref 7;;

let random() =
  seed := 175627 * !seed + 93857;
  !seed
;;

let generate fd seed =
  let chan = out_channel_of_descr fd in
  let s = ref seed in
    while true do
      output_binary_int chan !s;
      s := 25173 * !s + 13469
    done
;;

let check fd seed =
  let chan = in_channel_of_descr fd in
  let s = ref seed in
  let pid = getpid() in
    while true do
      let m = input_binary_int chan in
      if m = !s then begin
        print_int pid; print_string " Okay"; print_newline()
      end else begin
        print_int pid; print_string " Mismatch"; print_newline();
        s := m; ()
      end;
      s := 25173 * !s + 13469;
      sleep(1)
    done
;;

let proc1 channel1 =
  let input = make_vect nprocs stdin in
    for i = 0 to nprocs-1 do
      let (p_read, p_write) = pipe() in
      let seed = random() in
      match fork() with
        0 -> close p_read; generate p_write seed
      | _ -> close p_write; input.(i) <- p_read; ()
    done;
    multiplex channel1 input [||]
;;

let proc2 channel2 =
  let output = make_vect nprocs stdout in
    for i = 0 to nprocs-1 do
      let (p_read, p_write) = pipe() in
      let seed = random() in
      match fork() with
        0 -> close p_write; check p_read seed
      | _ -> close p_read; output.(i) <- p_write; ()
    done;
    multiplex channel2 [||] output
;;

let main() =
  let (channel1, channel2) = socketpair PF_UNIX SOCK_STREAM 0 in
    match fork() with
      0 -> handle_unix_error proc1 channel1
    | _ -> handle_unix_error proc2 channel2
;;

handle_unix_error main ();;    
