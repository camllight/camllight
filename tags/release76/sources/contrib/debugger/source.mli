(************************ Source management ****************************)

(*** Conversion function. ***)

value source_of_module: string -> string;;

(*** Buffer cache ***)

type BUFFER;;

value buffer_max_count : int ref;;

value flush_buffer_list : unit -> unit;;

value get_buffer : string -> BUFFER;;

value buffer_content : BUFFER -> string;;
value buffer_length : BUFFER -> int;;

(*** Position conversions. ***)

(* Pair (position, line) where `position' is the position in character of *)
(* the beginning of the line (first character is 0) and `line' is its *)
(* number (first line number is 1). *)
type POSITION == int * int;;

(* Position of the next linefeed after `pos'. *)
(* Position just after the buffer end if no linefeed found. *)
(* Raise `Out_of_range' if already there. *)
value next_linefeed : BUFFER -> int -> int;;

(* Go to next line. *)
value next_line : BUFFER -> POSITION -> POSITION;;

(* Convert a position in the buffer to a line number. *)
value line_of_pos : BUFFER -> int -> POSITION;;

(* Convert a line number to a position. *)
value pos_of_line : BUFFER -> int -> POSITION;;

(* Convert a coordinate (line / column) into a position. *)
(* --- The first line and column are line 1 and column 1. *)
value point_of_coord : BUFFER -> int -> int -> int;;
