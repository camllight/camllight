#open "test";;

print_newline();;
if !error_occurred then begin
  prerr_endline "************* TEST FAILED ****************"; exit 2
 end else begin
  prerr_endline "************* TEST COMPLETED SUCCESSFULLY ****************"; exit 0
end
;;
