(* Extract all strings given to interntl__e?printf in a source file *)

{
let char_for_decimal_code lexbuf i =
  char_of_int(100 * (int_of_char(get_lexeme_char lexbuf i) - 48) +
               10 * (int_of_char(get_lexeme_char lexbuf (i+1)) - 48) +
                    (int_of_char(get_lexeme_char lexbuf (i+2)) - 48))
;;

let interntl_is_open = ref false;;
}

rule main = parse
    "#open" [` ` `\010` `\013` `\009` `\012`] * "\"interntl\""
      { interntl_is_open := true; main lexbuf }
  | "interntl__" ( `e`? "printf" | "translate")
    [` ` `\010` `\013` `\009` `\012`] * `"`
      { string lexbuf; main lexbuf }
  | `e`? "printf" [` ` `\010` `\013` `\009` `\012`] * `"`
      { if !interntl_is_open then string lexbuf;
        main lexbuf }
  | "fprintf" [` ` `\010` `\013` `\009` `\012`] *
    [`a`-`z``A`-`Z`] + [` ` `\010` `\013` `\009` `\012`] * `"`
      { if !interntl_is_open then string lexbuf;
        main lexbuf }
  | eof { () }
  | _ { main lexbuf }

and string = parse
    `"`
      { print_string "\n\n" }
  | "\\\n" [` ` `\t`] *
      { print_string "\\\n"; string lexbuf }
  | "\\\""
      { print_string "\"";
        string lexbuf }
  | eof
      { prerr_endline "Warning: string not terminated" }
  | _
      { print_char(get_lexeme_char lexbuf 0);
        string lexbuf }
;;

