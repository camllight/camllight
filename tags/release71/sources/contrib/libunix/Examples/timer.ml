(* To time a function application, and print the time *)

#open "unix";;
#open "printf";;

let time fct arg =
  let start_time = unix__time() and start_ptimes = unix__times() in
  let res = fct arg in
  let stop_time = unix__time() and stop_ptimes = unix__times() in
    printf "User: %.2f    System: %.2f    Real: %d"
           (stop_ptimes.tms_utime -. start_ptimes.tms_utime)
           (stop_ptimes.tms_stime -. start_ptimes.tms_stime)
           (stop_time - start_time);
    print_newline();
    res
;;
