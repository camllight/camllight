(************************** Basic functions and types ***********************)

#open "unix";;

(*** Miscellaneous ***)
value ignore : 'a -> unit;;

(*** Types and exceptions. ***)

exception Out_of_range;;
exception Exit;;

(*** Operations on lists. ***)

(* Remove on element from an association list. *)
value assoc_remove : ('a * 'b) list -> 'a -> ('a * 'b) list;;

(* Nth element of a list. *)
value list_nth : 'a list -> int -> 'a;;

(* Return the `n' first elements of `l'. *)
(* ### n l -> l' *)
value list_truncate : int -> 'a list -> 'a list;;

(* Separe the `n' first elements of `l' and the others. *)
(* ### n list -> (first, last) *)
value list_truncate2 : int -> 'a list -> 'a list * 'a list;;

(* Replace x by y in list l *)
(* ### x y l -> l' *)
value list_replace : 'a -> 'a -> 'a list -> 'a list;;

(* Filter `list' according to `predicate'. *)
(* ### predicate list -> list' *)
value filter : ('a -> bool) -> 'a list -> 'a list;;

(* Find the first element `element' of `list' *)
(* so that `predicate element' holds. *)
(* Raise `Not_found' if no such element. *)
(* ### predicate list -> element *)
value find : ('a -> bool) -> 'a list -> 'a;;

(*** Operations on strings. ***)

(* Return the position of the first occurence of char `c' in string `s' *)
(* Raise `Not_found' if `s' does not contain `c'. *)
(* ### c s -> pos *)
value string_pos : string -> char -> int;;

(* Remove blanks (spaces and tabs) at beginning and end of a string. *)
value string_trim : string -> string;;

(*** I/O channels ***)

value skip_binary_int : in_channel -> unit;;

type IO_CHANNEL =
  {Io_in : in_channel;
   Io_out : out_channel;
   Io_fd : file_descr};;

value io_channel_of_descr : file_descr -> IO_CHANNEL;;
value close_io : IO_CHANNEL -> unit;;
value std_io :IO_CHANNEL;;

(*** Formatting ***)
value print_word : string -> unit;;

value message : string -> unit;;
