(* The Fibonacci function, once more. *)

let rec fib n =
  if n < 2 then 1 else fib(n-1) + fib(n-2)
;;

if sys__interactive then () else
if vect_length sys__command_line <> 2 then begin
  print_string "Usage: fib <number>";
  print_newline()
end else begin
  try
    print_int(fib(int_of_string sys__command_line.(1)));
    print_newline()
  with Failure "int_of_string" ->
    print_string "Bad integer constant";
    print_newline()
end
;;
