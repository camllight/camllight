#open "sys";;
#open "unix";;

let main () =
  let hh = int_of_string (sub_string command_line.(1) 0 2)
  and mm = int_of_string (sub_string command_line.(1) 2 2) in
  let now = localtime(time()) in
  let delay = (hh - now.tm_hour) * 3600 + (mm - now.tm_min) * 60 in
  if delay <= 0 then begin
    print_string "Hey! That time has already passed!";
    print_newline();
    exit 0
  end;
  if fork() <> 0 then exit 0;
  sleep delay;
  print_string "\007\007\007Time to leave!";
  print_newline();
  exit 0;;

handle_unix_error main ();;
