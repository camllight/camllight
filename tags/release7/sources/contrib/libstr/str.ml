#open "istr";;

let string_before s n =
  if n == 0 then s else sub_string s 0 n;;

let string_after s n = sub_string s n (string_length s - n);;

let first_chars s n = sub_string s 0 n;;

let last_chars s n = sub_string s (string_length s - n) n;;

let concat sl =
  let len = it_list (fun l s -> l + string_length s) 0 sl in
  let res = create_string len in
  it_list (fun pos s ->
             let l = string_length s in
             blit_string s 0 res pos l; pos+l)
          0 sl;
  res;;

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
      it_list (fun pos s ->
                 blit_string sep 0 res pos seplen;
                 let l = string_length s in
                 blit_string s 0 res (pos+seplen) l;
                 pos+seplen+l)
              hdlen tl;
      res;;

let regexp e = compile_regexp e false;;

let regexp_case_fold e = compile_regexp e true;;

let group_beginning n =
  if n < 0 or n >= 10 then invalid_arg "str__group_beginning" else
  let pos = beginning_group n in
  if pos == -1 then raise Not_found else pos;;

let group_end n =
  if n < 0 or n >= 10 then invalid_arg "str__group_end" else
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

#open "obj";;

let format format =
  let format = (magic format : string) in
  let res = ref [] in

  let rec doprn start i =
    if i >= string_length format then begin
      if i > start then res := sub_string format start (i-start) :: !res;
      magic(concat(rev !res))
    end else
      if nth_char format i != `%` then
        doprn start (i+1)
      else begin
        if i > start then res := sub_string format start (i-start) :: !res;
        let j = skip_args (succ i) in
        match nth_char format j with
          `%` ->
            doprn j (succ j)
        | `s` ->
            magic(dostring i j)
        | `c` ->
            magic(fun c ->
                res := make_string 1 c :: !res;
                doprn (succ j) (succ j))
        | `d` | `i` | `o` | `x` | `X` | `u` ->
            magic(doint i j)
        | `f` | `e` | `E` | `g` | `G` ->
            magic(dofloat i j)
        | `b` ->
            magic(fun b ->
                res := string_of_bool b :: !res;
                doprn (succ j) (succ j))
        | `a` ->
            magic(fun printer arg ->
              res := printer () arg :: !res;
              doprn (succ j) (succ j))
        | `t` ->
            magic(fun printer ->
              res := printer () :: !res;
              doprn (succ j) (succ j))
        | c ->
            invalid_arg ("format: unknown format " ^ char_for_read c)
      end

  and skip_args j =
    match nth_char format j with
      `0` | `1` | `2` | `3` | `4` | `5` | `6` | `7` | `8` | `9` |
      ` ` | `.` | `-` ->
        skip_args (succ j)
    | c ->
        j
    
  and dostring i j s =
    if j <= i+1 then
      res := s :: !res
    else begin
      let p =
        try
          int_of_string (sub_string format (i+1) (j-i-1))
        with _ ->
          invalid_arg "format: bad %s format" in
      if p > 0 & string_length s < p then begin
        res := make_string (p - string_length s) ` ` :: !res;
        res := s :: !res
      end else if p < 0 & string_length s < -p then begin
        res := s :: !res;
        res := make_string (-p - string_length s) ` ` :: !res
      end else
        res := s :: !res
    end;
    doprn (succ j) (succ j)

  and doint i j n =
    let len = j-i in
    let fmt = create_string (len+2) in
    blit_string format i fmt 0 len;
    set_nth_char fmt len `l`;
    set_nth_char fmt (len+1) (nth_char format j);
    res := format_int fmt n :: !res;
    doprn (succ j) (succ j)

  and dofloat i j f =
    res := format_float (sub_string format i (j-i+1)) f :: !res;
    doprn (succ j) (succ j)

  in doprn 0 0
;;
