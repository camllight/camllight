#open "sys";;
#open "unix";;
let main () =
  execvp "grep"
         (concat_vect [|"grep"; "-i"|]
                      (sub_vect command_line 1
                                (vect_length command_line - 1)))
;;
handle_unix_error main ();;
