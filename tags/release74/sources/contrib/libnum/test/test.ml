#open "printf";;

let error_occurred = ref false;;

let function_tested = ref "";;

let testing_function s =
    flush std_out;
    flush std_err;
    function_tested := s;
    print_newline();
    print_string s;
    print_newline();;

let test test_number eq_fun (answer, correct_answer) =
 flush std_out;
 flush std_err;
 if not eq_fun answer correct_answer then begin
   fprintf stderr ">>> Bad result (%s, test %d)\n" !function_tested test_number;
   error_occurred := true;
   false
 end else begin
   printf " %d..." test_number;
   true
 end
;;

let failure_test test_number fun_to_test arg =
 flush std_out;
 flush std_err;
 try
   fun_to_test arg;
   fprintf stderr ">>> Failure expected (%s, test %d)\n"
                  !function_tested test_number;
   error_occurred := true;
   false
  with _ ->
   printf " %d..." test_number;
   true
;;

let failwith_test test_number fun_to_test arg correct_failure =
 flush std_out;
 flush std_err;
 try
   fun_to_test arg;
   fprintf stderr ">>> Failure expected (%s, test %d)\n"
                  !function_tested test_number;
   error_occurred := true;
   false
  with x ->
   if x = correct_failure then begin
     printf " %d..." test_number;
     true
   end else begin
     fprintf stderr ">>> Bad failure (%s, test %d)\n"
                    !function_tested test_number;
     error_occurred := true;
     false
   end   
;;

let eq = prefix ==;;
