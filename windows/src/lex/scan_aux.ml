(* Auxiliaries for the lexical analyzer *)

#open "lexing";;

let brace_depth = ref 0
and comment_depth = ref 0;;

exception Lexical_error of string;;

let initial_string_buffer = create_string 256;;
let string_buff = ref initial_string_buffer;;
let string_index = ref 0;;

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0
;;

let store_string_char c =
  (if !string_index >= string_length (!string_buff) then
    let new_buff = create_string (string_length (!string_buff) * 2) in
      blit_string new_buff 0 (!string_buff) 0 (string_length (!string_buff));
      string_buff := new_buff;
      ());
  set_nth_char (!string_buff) (!string_index) c;
  incr string_index
;;

let get_stored_string () =
  let s = sub_string (!string_buff) 0 (!string_index) in
    string_buff := initial_string_buffer;
    s
;;

let char_for_backslash = function
    `n` -> `\n`  (* do NOT put explicit numeric values here -- DD *)
  | `t` -> `\t`
  | `b` -> `\b`
  | `r` -> `\r`
  | c   -> c
;;

let char_for_decimal_code lexbuf i =
  char_of_int(100 * (int_of_char(get_lexeme_char lexbuf i) - 48) +
               10 * (int_of_char(get_lexeme_char lexbuf (i+1)) - 48) +
                    (int_of_char(get_lexeme_char lexbuf (i+2)) - 48))
;;
