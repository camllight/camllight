(* Operations on strings, without sanity checks *)

#open "int";;
#open "eq";;
#open "ref";;
#open "fchar";;
#open "exc";;

let make_string len init =
  let s = create_string len in
    fill_string s 0 len init; s
;;

let prefix ^ s1 s2 =
  let l1 = string_length s1
  and l2 = string_length s2 in
  let s = create_string (l1 + l2) in
    blit_string s1 0 s 0 l1;
    blit_string s2 0 s l1 l2;
    s
;;

let concat sl =
  let rec concat_len l = function
    [] -> l
  | s :: r -> concat_len (l + string_length s) r in
  let res = create_string (concat_len 0 sl) in
  let rec fill_res pos = function
    [] -> ()
  | s :: r ->
      let l = string_length s in
      blit_string s 0 res pos l;
      fill_res (pos+l) r in
  fill_res 0 sl;
  res;;

let sub_string s start len =
  let res = create_string len in
    blit_string s start res 0 len; res
;;

let replace_string dest src pos =
  blit_string src 0 dest pos (string_length src)
;;

let string_for_read s =
  let n = ref 0 in
    for i = 0 to string_length s - 1 do
      n := !n +
        (match nth_char s i with
           `"` | `\\` | `\n` | `\t` -> 2
          | c -> if is_printable c then 1 else 4)
    done;
    if !n == string_length s then s else begin
      let s' = create_string !n in
        n := 0;
        for i = 0 to string_length s - 1 do
          begin
            match nth_char s i with
              `"` -> set_nth_char s' !n `\\`; incr n; set_nth_char s' !n `"`
            | `\\` -> set_nth_char s' !n `\\`; incr n; set_nth_char s' !n `\\`
            | `\n` -> set_nth_char s' !n `\\`; incr n; set_nth_char s' !n `n`
            | `\t` -> set_nth_char s' !n `\\`; incr n; set_nth_char s' !n `t`
            | c ->
                if is_printable c then
                  set_nth_char s' !n c
                else begin
                  let a = int_of_char c in
                  set_nth_char s' !n `\\`;
                  incr n;
                  set_nth_char s' !n (char_of_int (48 + a / 100));
                  incr n;
                  set_nth_char s' !n (char_of_int (48 + (a / 10) mod 10));
                  incr n;
                  set_nth_char s' !n (char_of_int (48 + a mod 10))
                end
          end;
          incr n
        done;
        s'
      end
;;

let rec index_char_from s i c =
  if i >= string_length s then raise Not_found
  else if s.[i] = c then i
  else index_char_from s (i+1) c
;;

let index_char s c = index_char_from s 0 c
;;

let rec rindex_char_from s i c =
  if i < 0 then raise Not_found
  else if s.[i] = c then i
  else rindex_char_from s (i-1) c
;;

let rindex_char s c = rindex_char_from s (string_length s - 1) c
;;
