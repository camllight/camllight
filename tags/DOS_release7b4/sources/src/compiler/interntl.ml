(* Internationalization (translation of error messages) *)

#open "misc";;

let language = ref "";;

let read_transl_file msgfile =
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
    | c             -> read_tag(store_tag c i) in
  let transl_tbl = hashtbl__new 37 in
  let currsrc = ref "" in
  begin try
    while true do
      let (tag_len, msg_len) = read_tag 0 in
      if sub_string tag_buffer 0 tag_len = "src" then
        currsrc := sub_string msg_buffer 0 msg_len
      else if sub_string tag_buffer 0 tag_len = !language then
        hashtbl__add transl_tbl !currsrc (sub_string msg_buffer 0 msg_len)
      else ()
    done
  with End_of_file ->
    close_in ic
  end;
  transl_tbl
;;

type translation_table =
    Unknown
  | None
  | Transl of (string, string) hashtbl__t;;

let transl_table = ref Unknown;;

let rec translate msg =
  match !transl_table with
    None ->
      msg
  | Transl tbl ->
      begin try hashtbl__find tbl msg with Not_found -> msg end
  | Unknown ->
      transl_table :=
        if string_length !language == 0 then
          None
        else begin
          try
            Transl(read_transl_file(find_in_path "camlmsgs.txt"))
          with Cannot_find_file _ | sys__Sys_error _ | sys__Break ->
            None
        end;
      translate msg
;;

let fprintf oc (fmt : ('a, out_channel, unit) printf__format) =
  printf__fprintf oc
    (obj__magic(translate(obj__magic fmt : string)) :
                                ('a, out_channel, unit) printf__format)
;;

let printf fmt = fprintf std_out fmt
and eprintf fmt = fprintf std_err fmt
;;

