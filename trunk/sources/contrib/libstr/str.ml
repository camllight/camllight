#open "istr";;

let string_before s n = sub_string s 0 n;;

let string_after s n = sub_string s n (string_length s - n);;

let first_chars s n = sub_string s 0 n;;

let last_chars s n = sub_string s (string_length s - n) n;;

let concat = string__concat;;

let join sep = function
    [] -> ""
  | hd::tl ->
      let seplen = string_length sep in
      let hdlen = string_length hd in
      let len =
        list_length tl * seplen +
        it_list (fun l s -> l + string_length s) hdlen tl in
      let res = create_string len in
      blit_string hd 0 res 0 hdlen;
      let _ = 
      it_list (fun pos s ->
                 blit_string sep 0 res pos seplen;
                 let l = string_length s in
                 blit_string s 0 res (pos+seplen) l;
                 pos+seplen+l)
              hdlen tl in
      res;;

let regexp e = compile_regexp e false;;

let regexp_case_fold e = compile_regexp e true;;

let group_beginning n =
  if n < 0 || n >= 10 then invalid_arg "str__group_beginning" else
  let pos = beginning_group n in
  if pos == -1 then raise Not_found else pos;;

let group_end n =
  if n < 0 || n >= 10 then invalid_arg "str__group_end" else
  let pos = end_group n in
  if pos == -1 then raise Not_found else pos;;

let matched_group n txt =
  let b = group_beginning n and e = group_end n in sub_string txt b (e-b);;

let match_beginning () = group_beginning 0
and match_end () = group_end 0
and matched_string txt = matched_group 0 txt;;

let substitute_first expr repl_fun text =
  try
    let pos = search_forward expr text 0 in
    concat [string_before text pos; 
            repl_fun text;
            string_after text (match_end())]
  with Not_found ->
    text
;;

let global_substitute expr repl_fun text =
  let rec replace start =
    try
      let pos = search_forward expr text start in
      let repl_text = repl_fun text in
      sub_string text start (pos-start) ::
      repl_text ::
      replace (match_end())
    with Not_found ->
      [string_after text start] in
  concat (replace 0)
;;

let global_replace expr repl text =
  global_substitute expr (replacement_text repl) text
and replace_first expr repl text =
  substitute_first expr (replacement_text repl) text
;;

let bounded_split expr text num =
  let start =
    if string_match expr text 0 then match_end() else 0 in
  let rec split start n =
    if start >= string_length text then [] else
    if n == 1 then [string_after text start] else
      try
        let pos = search_forward expr text start in
        sub_string text start (pos-start) :: split (match_end()) (n-1)
      with Not_found ->
        [string_after text start] in
  split start num
;;

let split expr text = bounded_split expr text 0;;

let format = printf__sprintf;;
