#open "sys";;
#open "unix";;

let buffer_size = 8192;;
let buffer = create_string buffer_size;;

let file_copy input_name output_name =
  let fd_in = open input_name [O_RDONLY] 0 in
  let fd_out = open output_name [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in
  let rec copy_loop () =
    match read fd_in buffer 0 buffer_size with
      0 -> ()
    | r -> write fd_out buffer 0 r; copy_loop () in
  copy_loop ();
  close fd_in;
  close fd_out;;

let main () =
if vect_length command_line = 3 then begin
  file_copy command_line.(1) command_line.(2);
  exit 0
end else begin
  prerr_endline "Usage: file_copy <input_file> <output_file>";
  exit 1
end;;

handle_unix_error main ();;
