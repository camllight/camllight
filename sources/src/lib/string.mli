(* String operations *)

value string_length : string -> int = 1 "string_length"
        (* Return the length (number of characters) of the given string. *)
;;
value nth_char : string -> int -> char
        (* [nth_char s n] returns character number [n] in string [s].
           The first character is character number 0.
           The last character is character number [string_length s - 1].
           Raise [Invalid_argument "nth_char"] if [n] is ouside the range
           0 -- [(string_length s - 1)]. *)
  and set_nth_char : string -> int -> char -> unit
        (* [set_nth_char s n c] modifies string [s] in place,
           replacing the character number [n] by [c].
           Raise [Invalid_argument "set_nth_char"] if [n] is ouside the range
           0 to [(string_length s - 1)]. *)
;;
value prefix ^ : string -> string -> string
        (* [s1 ^ s2] returns a fresh string containing the concatenation of
           the strings [s1] and [s2]. *)
  and sub_string : string -> int -> int -> string
        (* [sub_string s start len] returns a fresh string of length [len],
           containing the characters number [start] to [start + len - 1]
           of string [s].
           Raise [Invalid_argument "sub_string"] if [start] and [len] do not
           designate a valid substring of [s]; that is, if [start < 0],
           or [len < 0], or [start + len > string_length s]. *)
;;
value create_string : int -> string
        (* [create_string n] returns a fresh string of length [n].
           The string initially contains arbitrary characters. *)
  and make_string : int -> char -> string
        (* [make_string n c] returns a fresh string of length [n],
           filled with the character [c]. *)
;;
value fill_string : string -> int -> int -> char -> unit
        (* [fill_string s start len c] modifies string [s] in place,
           replacing the characters number [start] to [start + len - 1]
           by [c].
           Raise [Invalid_argument "fill_string"] if [start] and [len] do not
           designate a valid substring of [s]. *)
  and blit_string : string -> int -> string -> int -> int -> unit
        (* [blit_string s1 o1 s2 o2 len] copies [len] characters
           from string [s1], starting at character number [o1], to string [s2],
           starting at character number [o2]. It works correctly even if
           [s1] and [s2] are the same string,
           and the source and destination chunks overlap.
           Raise [Invalid_argument "blit_string"] if [o1] and [len] do not
           designate a valid substring of [s1], or if [o2] and [len] do not
           designate a valid substring of [s2]. *)
  and replace_string : string -> string -> int -> unit
        (* [replace_string dest src start] copies all characters from the 
           string [src] into the string [dst], starting at
           character number [start] in [dst].
           Raise [Invalid_argument "replace_string"] if copying would overflow
           string [dest]. *)
;;
value eq_string : string -> string -> bool = 2 "=string"
  and neq_string : string -> string -> bool = 2 "<>string"
  and le_string : string -> string -> bool = 2 "<=string"
  and lt_string : string -> string -> bool = 2 "<string"
  and ge_string : string -> string -> bool = 2 ">=string"
  and gt_string : string -> string -> bool = 2 ">string"
        (* Comparison functions (lexicographic ordering) between strings. *)
;;
value compare_strings : string -> string -> int = 2 "compare_strings"
        (* General comparison between strings.
	   [compare_strings s1 s2] returns 0 if [s1] and [s2] are equal,
	   or else -2 if [s1] is a prefix of [s2],
	   or 2 if [s2] is a prefix of [s1],
	   or else -1 if [s1] is lexicographically before [s2],
	   or 1 if [s2] is lexicographically before [s1]. *)
;;
value string_for_read : string -> string
        (* Return a copy of the argument, with special characters represented
           by escape sequences, following the lexical conventions of
           Caml Light. *)
;;
