(* Character operations, without sanity checks *)

#open "int";;
#open "fstring";;

let char_for_read = function
    `\`` -> "\\`"
  | `\\` -> "\\\\"
  | `\n` -> "\\n"
  | `\t` -> "\\t"
  | c ->  if is_printable c then
            make_string 1 c
          else begin
            let n = int_of_char c in
            let s = create_string 4 in
            set_nth_char s 0 `\\`;
            set_nth_char s 1 (char_of_int (48 + n / 100));
            set_nth_char s 2 (char_of_int (48 + (n / 10) mod 10));
            set_nth_char s 3 (char_of_int (48 + n mod 10));
            s
          end
;;
