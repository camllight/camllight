(************************** Basic functions and types ***********************)

#open "unix";;

(*** Miscellaneous ***)
let nothing _ = ();;

(*** Operations on lists. ***)

(* Remove on element from an association list *)
let assoc_remove lst elem =
  let rec remove =
    function
      [] -> []
    | ((a, _) as c::t) ->
      if a = elem then t
      else c::(remove t)
  in remove lst;;

(* Return the `n' first elements of `l' *)
(* ### n l -> l' *)
let rec list_truncate =
  fun
    0 _      -> []
  | _ []     -> []
  | n (a::l) -> a::(list_truncate (n - 1) l);;

(* Separe the `n' first elements of `l' and the others *)
(* ### n list -> (first, last) *)
let rec list_truncate2 =
  fun
    0 l ->
      ([], l)
  | _ [] ->
      ([], [])
  | n (a::l) ->
      let (first, last) = (list_truncate2 (n - 1) l) in
        (a::first, last);;

(* Replace x by y in list l *)
(* ### x y l -> l' *)
let list_replace x y =
  let rec repl =
    function
      [] -> []
    | a::l ->
        if a == x then y::l
        else a::(repl l)
  in repl;;

(* Filter `list' according to `predicate'. *)
(* ### predicate list -> list' *)
let filter p =
  let rec filter2 =
    function
      [] ->
        []
    | a::l ->
        if p a then
	  a::(filter2 l)
        else
      	  filter2 l
  in filter2;;

(* Find the first element `element' of `list' *)
(* so that `predicate element' holds. *)
(* ### predicate list -> element *)
let find p =
  let rec find2 =
    function
      [] ->
      	raise Not_found
    | a::l ->
      	if p a then a
	else find2 l
  in find2;;

(*** Operations on strings. ***)

(* Return the position of the first occurence of char `c' in string `s' *)
(* Raise `Not_found' if `s' does not contain `c'. *)
(* ### c s -> pos *)
let string_pos s c =
  let i = ref 0 and l = string_length s in
    while (!i < l) & (nth_char s !i != c) do i := !i + 1 done;
    if !i = l then raise Not_found;
    !i;;

(* Remove blanks (spaces and tabs) at beginning and end of a string. *)
let string_trim s =
  let l = string_length s and i = ref 0 in
    while
      (!i < l) & (match nth_char s !i with ` ` | `\t` -> true | _ -> false)
    do
      incr i
    done;
    let j = ref (l - 1) in
      while
        (!j >= !i) & (match nth_char s !j with ` ` | `\t` -> true | _ -> false)
      do
        decr j
      done;
      sub_string s !i (!j - !i + 1);;

(*** I/O channels ***)

let io_channel_of_descr fd =
  {Io_in = in_channel_of_descr fd;
   Io_out = out_channel_of_descr fd;
   Io_fd = fd};;

let close_io io_channel =
  (try
     (try
        close_out io_channel.Io_out
      with
      	sys__Sys_error _ -> ())
   with End_of_file -> ());		(* SIGPIPE during flush. *)
  close_in io_channel.Io_in;;

let std_io = {Io_in = std_in; Io_out = std_out; Io_fd = stdin};;

(*** Path expansion. ***)

(* Expand a path. *)
(* ### path -> path' *)
let rec expand_path ch =
  let rec subst_variable ch =
    try
      let pos = string_pos ch `$` in
        if (pos + 1 < string_length ch) & (nth_char ch (pos + 1) = `$`) then
      	  (sub_string ch 0 (pos + 1))
	    ^ (subst_variable
      	         (sub_string ch (pos + 2) (string_length ch - pos - 2)))
        else
          (sub_string ch 0 pos)
            ^ (subst2 (sub_string ch (pos + 1) (string_length ch - pos - 1)))
    with Not_found ->
      ch
  and subst2 ch =
    let suiv =
      let i = ref 0
      and stream = stream_of_string ch
      in
        while
          (function
             [< '`a`..`z` >] -> true
           | [< '`A`..`Z` >] -> true
           | [< '`0`..`9` >] -> true
           | [< '`_` >] -> true
           | [< '_ >] -> false
           | [< >] -> false) stream
        do i := !i + 1 done;
        !i
    in (sys__getenv (sub_string ch 0 suiv))
       ^ (subst_variable (sub_string ch suiv (string_length ch - suiv)))
  in
    let ch = subst_variable ch in
      let concat_root nom ch2 =
        try filename__concat (getpwnam nom).pw_dir ch2
        with Failure "Not found" ->
          raise (Invalid_argument "expand_path")
      in
        if (nth_char ch 0) = `~` then
	  try
            match string_pos ch `/` with
              1 ->
                (let tail = sub_string ch 2 (string_length ch - 2)
                 in
                   try filename__concat (sys__getenv "HOME") tail
                   with Failure "Not found" ->
                     concat_root (sys__getenv "LOGNAME") tail)
            |  n -> concat_root
                      (sub_string ch 1 (n - 1))
                      (sub_string ch (n + 1) (string_length ch - n - 1))
          with
	    Not_found ->
              expand_path (ch ^ "/")
        else ch;;
