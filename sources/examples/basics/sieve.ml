(* Eratosthene's sieve *)

(* interval min max = [min; min+1; ...; max-1; max] *)

let rec interval min max =
  if min > max then [] else min :: interval (succ min) max
;;

(* filter p L returns the list of the elements in list L
   that satisfy predicate p *)

let rec filter p = function
  | []  -> []
  | a::r -> if p a then a :: filter p r else filter p r
;;

(* Application: removing all numbers multiple of n from a list of integers *)

let remove_multiples_of n =
  filter (fun m -> m mod n <> 0)
;;

(* The sieve itself *)

let sieve max =
  let rec filter_again = function
  | [] -> []
  | n::r as l ->
      if n*n > max then l else n :: filter_again (remove_multiples_of n r)
  in
    filter_again (interval 2 max)
;;

(* The entry point *)

let usage() =
  print_string "Usage: sieve <n>\n";
  exit 2
;;

if sys__interactive then () else
if vect_length sys__command_line <> 2 then begin
  print_string "Usage: sieve <n>";
  print_newline()
end else begin
  try
    let n = int_of_string sys__command_line.(1) in
    do_list (fun n -> print_int n; print_string " ") (sieve n);
    print_newline()
  with Failure "int_of_string" ->
    print_string "Bad integer constant";
    print_newline()
end
;;
