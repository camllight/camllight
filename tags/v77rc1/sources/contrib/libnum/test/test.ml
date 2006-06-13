#open "printf";;

let flush_all () = flush std_out; flush std_err;;

let message s = print_string s; print_newline ();;

let error_occurred = ref false;;
let immediate_failure = ref true;;

let error () =
 if !immediate_failure then exit 2 else begin
   error_occurred := true; flush_all (); false
 end;;

let success () = flush_all (); true;;

let function_tested = ref "";;

let testing_function s =
    flush_all ();
    function_tested := s;
    print_newline();
    message s;;

let test test_number eq_fun (answer, correct_answer) =
 flush_all ();
 if not eq_fun answer correct_answer then begin
   fprintf stderr ">>> Bad result (%s, test %d)\n" !function_tested test_number;
   error ()
 end else begin
   printf " %d..." test_number;
   success ()
 end;;

let failure_test test_number fun_to_test arg =
 flush_all ();
 try
   fun_to_test arg;
   fprintf stderr ">>> Failure expected (%s, test %d)\n"
                  !function_tested test_number;
   error ()
  with _ ->
   printf " %d..." test_number;
   success ();;

let failwith_test test_number fun_to_test arg correct_failure =
 flush_all ();
 try
   fun_to_test arg;
   fprintf stderr ">>> Failure expected (%s, test %d)\n"
                  !function_tested test_number;
   error ()
  with x ->
   if x = correct_failure then begin
     printf " %d..." test_number;
     success ()
   end else begin
     fprintf stderr ">>> Bad failure (%s, test %d)\n"
                    !function_tested test_number;
     error ()
   end;;

let eq = prefix ==;;

let end_tests () =
 flush_all ();
 print_newline ();
 if !error_occurred then begin
   prerr_endline "************* TESTS FAILED ****************"; exit 2
 end else begin
   prerr_endline "************* TESTS COMPLETED SUCCESSFULLY ****************";
   exit 0
 end;;
