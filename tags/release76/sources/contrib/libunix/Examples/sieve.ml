#open "unix";;

let generate output =
  let rec gen m =
    output_binary_int output m;
    gen (m+1)
  in gen 2;;

let read_first_primes input count =
  let rec read_primes first_primes count =
    if count <= 0 then
      first_primes
    else
      let n = input_binary_int input in
      if exists (fun m -> n mod m = 0) first_primes then
        read_primes first_primes count
      else begin
        print_int n; print_newline();
        read_primes (n :: first_primes) (count - 1)
      end in
  read_primes [] count
;;

let rec filter input =
  let first_primes = read_first_primes input 1000 in
  let (fd_out, fd_in) = pipe() in
  match fork() with
    0 ->
      close fd_in;
      filter (in_channel_of_descr fd_out)
  | _ ->
      close fd_out;
      let output = out_channel_of_descr fd_in in
      while true do
        let n = input_binary_int input in
        if exists (fun m -> n mod m = 0) first_primes then
          ()
        else begin
          print_int n; print_newline();
          output_binary_int output n
        end
      done
;;

let main () =
  let (fd_out, fd_in) = pipe () in
  match fork() with
    0 ->
      close fd_in;
      filter (in_channel_of_descr fd_out)
  | _ ->
      close fd_out;
      generate (out_channel_of_descr fd_in)
;;
printexc__f main ();;
