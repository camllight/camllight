try
  let lexbuf = lexing__create_lexer_channel std_in in
  while true do
    let result = parser__Main lexer__Token lexbuf in
      print_int result; print_newline(); flush std_out
  done
with _ ->
  ()
;;
