(* Internationalization (translation of error messages) *)

#open "misc";;

let language = ref "";;

let translate msg =
  if string_length !language == 0 then msg else begin
    try
      let msgfile = find_in_path "camlmsgs.txt" in
      let ic = open_in msgfile in
      let tag_buffer = create_string 16
      and msg_buffer = create_string 1024 in
      let rec store_tag c i =
        if i >= 16 then i else (tag_buffer.[i] <- c; succ i)
      and store_msg c i =
        if i >= 1024 then i else (msg_buffer.[i] <- c; succ i)
      and read_line i =
        match input_char ic with
          `\n` -> i
        | `\\` -> begin match input_char ic with
                    `\\` -> read_line(store_msg `\\` i)
                  | `n`  -> read_line(store_msg `\n` i)
                  | `\n` -> skip_blanks i
                  | c    -> read_line(store_msg c (store_msg `\\` i))
                  end
        | c    -> read_line(store_msg c i)
      and skip_blanks i =
        match input_char ic with
          ` `|`\t` -> skip_blanks i
        | c        -> read_line(store_msg c i)
      and read_tag i =
        match input_char ic with
          `:`           -> (i, skip_blanks 0)
        | ` `|`\n`|`\t` -> read_tag i
        | c             -> read_tag(store_tag c i)
      and find_translation () =
        let (tag_len, msg_len) = read_tag 0 in
        let tag = sub_string tag_buffer 0 tag_len in
        if tag = !language then sub_string msg_buffer 0 msg_len
        else if tag = "src" then msg (* No translation in the right language *)
        else find_translation()
      and find_message () =
        let (tag_len, msg_len) = read_tag 0 in
        if sub_string tag_buffer 0 tag_len = "src"
         & sub_string msg_buffer 0 msg_len = msg
        then find_translation()
        else find_message()
      in
      try
        let transl = find_message() in close_in ic; transl
      with End_of_file -> (* Message is not in message file *)
        close_in ic; msg
    with Cannot_find_file _ ->
      msg
  end
;;

let fprintf oc (fmt : ('a, out_channel, unit) printf__format) =
  printf__fprintf oc
    (obj__magic(translate(obj__magic fmt : string)) :
                                ('a, out_channel, unit) printf__format)
;;

let printf fmt = fprintf std_out fmt
and eprintf fmt = fprintf std_err fmt
;;

