type t == string;;

let empty size =
  make_string ((size + 7) lsr 3) `\000`;;

let singleton size n =
  let s = empty size in
  set_nth_char s (n lsr 3) (char_of_int (1 lsl (n land 7)));;

let union s1 s2 =
  let s = create_string (string_length s1) in
  for i = 0 to pred(string_length s1) do
    set_nth_char s i (char_of_int (lor (int_of_char (nth_char s1 i))
                                       (int_of_char (nth_char s2 i))))
  done;
  s;;

let apply f s =
  for i = 0 to pred(string_length s1) do
    let b = int_of_char (nth_char s i) in
    let n = i lsl 3 in
    if b land 1 != 0 then f n;
    if b land 2 != 0 then f (n+1);
    if b land 4 != 0 then f (n+2);
    if b land 8 != 0 then f (n+3);
    if b land 16 != 0 then f (n+4);
    if b land 32 != 0 then f (n+5);
    if b land 64 != 0 then f (n+6);
    if b land 128 != 0 then f (n+7)
  done
;;

exception Found of int;;

let min s =
  try
    apply (fun n -> raise (Found n)) s; (-1)
  with Found n ->
    n
;;


