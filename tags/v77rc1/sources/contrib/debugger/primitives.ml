(************************** Basic functions and types ***********************)

#open "unix";;

(*** Miscellaneous ***)
let ignore _ = ();;

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

(* Nth element of a list. *)
let rec list_nth =
  fun
    [] _ ->
      raise (Invalid_argument "list_nth")
  | (a::_) 0 ->
      a
  | (_::l) n ->
      list_nth l (n - 1);;

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
    while (!i < l) && (nth_char s !i != c) do i := !i + 1 done;
    if !i = l then raise Not_found;
    !i;;

(* Remove blanks (spaces and tabs) at beginning and end of a string. *)
let string_trim s =
  let l = string_length s and i = ref 0 in
    while
      (!i < l) && (match nth_char s !i with ` ` | `\t` -> true | _ -> false)
    do
      incr i
    done;
    let j = ref (l - 1) in
      while
        (!j >= !i) && (match nth_char s !j with ` ` | `\t` -> true | _ -> false)
      do
        decr j
      done;
      sub_string s !i (!j - !i + 1);;

(*** I/O channels ***)

let skip_binary_int inchan = ignore (input_binary_int inchan);;

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

(*** Formatting ***)
#open "format";;

let print_word s = print_string s; print_space();;

let message s = print_string s; print_newline();;
