(* Operations on streams *)

#open "io";;

type 'a stream;;
  (* The type of streams containing values of type ['a]. *)

exception Parse_failure;;
  (* Raised by parsers when none of the first component of the stream patterns
     is accepted *)
exception Parse_error;;
  (* Raised by parsers when the first component of a stream pattern is
     accepted, but one of the following components is rejected *)

value stream_next : 'a stream -> 'a;;
  (* [stream_next s] returns the first element of stream [s],
     and removes it from the stream. Raise [Parse_failure] if the
     stream is empty. *)

value stream_from : (unit -> 'a) -> 'a stream;;
  (* [stream_from f] returns the stream which fetches its terminals using
     the function [f]. This function could be defined as:
-    [    let rec stream_from f = [< 'f(); stream_from f >]]
-    but is implemented more efficiently. *)

value stream_of_string : string -> char stream;;
  (* [stream_of_string s] returns the stream of the characters in
     string [s]. *)

value stream_of_channel : in_channel -> char stream;;
  (* [stream_of_channel ic] returns the stream of characters read on
     channel [ic]. *)

value do_stream : ('a -> unit) -> 'a stream -> unit;;
  (* [do_stream f s] scans the whole stream [s], applying the function [f]
     in turn to each terminal encountered *)

value stream_check : ('a -> bool) -> 'a stream -> 'a;;
  (* [stream_check p] returns the parser which returns the first terminal of
     the stream if the predicate [p] returns [true] on this terminal,
     and raises [Parse_failure] otherwise. *)

value end_of_stream : 'a stream -> unit;;
  (* Return [()] iff the stream is empty, and raise [Parse_failure]
     otherwise.  *)

value stream_get : 'a stream -> 'a * 'a stream;;
  (* [stream_get s] return the first element of the stream [s], and a stream
     containing the remaining elements of [s]. Raise [Parse_failure] if the
     stream is empty. The stream [s] is not modified. This function makes it
     possible to access a stream non-destructively. *)

(*-- The following functions are for internal use. *)

value stream_require : 'a stream -> 'a
  and parser_require : ('a stream -> 'a) -> 'a stream -> 'a
;;
