(* Pretty printing *)

(* This module implements a pretty-printing facility to format text
   within ``pretty-printing boxes''. The pretty-printer breaks lines
   at specified break hints, and indents lines according to the box structure.
*)

(* The behaviour of pretty-printing commands is unspecified
   if there is no opened pretty-printing box. *)

#open "io";;

(*** Boxes *)
value open_vbox : int -> unit;;
        (* [open_vbox d] opens a new pretty-printing box
           with offset [d]. 
           This box is ``vertical'': every break hint inside this
           box leads to a new line.
           When a new line is printed in the box, [d] is added to the
           current indentation. *)
value open_hbox : unit -> unit;;
        (* [open_hbox ()] opens a new pretty-printing box.
           This box is ``horizontal'': the line is not split in this box
           (new lines may still occur inside boxes nested deeper). *)
value open_hvbox : int -> unit;;
        (* [open_hovbox d] opens a new pretty-printing box
           with offset [d]. 
           This box is ``horizontal-vertical'': it behaves as an
           ``horizontal'' box if it fits on a single line,
           otherwise it behaves as a ``vertical'' box.
           When a new line is printed in the box, [d] is added to the
           current indentation. *)
value open_hovbox : int -> unit;;
        (* [open_hovbox d] opens a new pretty-printing box
           with offset [d]. 
           This box is ``horizontal or vertical'': break hints
           inside this box may lead to a new line, if there is no more room
           on the line to print the remainder of the box.
           When a new line is printed in the box, [d] is added to the
           current indentation. *)
value close_box : unit -> unit;;
        (* Close the most recently opened pretty-printing box. *)

(*** Formatting functions *)
value print_string : string -> unit;;
        (* [print_string str] prints [str] in the current box. *)
value print_as : int -> string -> unit;;
        (* [print_as len str] prints [str] in the
           current box. The pretty-printer formats [str] as if
           it were of length [len]. *)
value print_int : int -> unit;;
        (* Print an integer in the current box. *)
value print_float : float -> unit;;
        (* Print a floating point number in the current box. *)
value print_char : char -> unit;;
        (* Print a character in the current box. *)
value print_bool : bool -> unit;;
        (* Print an boolean in the current box. *)

(*** Break hints *)
value print_break : int * int -> unit;;
        (* Insert a break hint in a pretty-printing box.
           [print_break (nspaces, offset)] indicates that the line may
           be split (a newline character is printed) at this point,
           if the contents of the current box does not fit on one line.
           If the line is split at that point, [offset] is added to
           the current indentation. If the line is not split,
           [nspaces] spaces are printed. *)
value print_cut : unit -> unit;;
        (* [print_cut ()] is equivalent to [print_break (0,0)].
           This allows line splitting at the current point, without printing
           spaces or adding indentation. *)
value print_space : unit -> unit;;
        (* [print_space ()] is equivalent to [print_break (1,0)].
           This either prints one space or splits the line at that point. *)
value force_newline : unit -> unit;;
        (* Force a newline in the current box. *)

value print_flush : unit -> unit;;
        (* Flush the pretty printer: all opened boxes are closed,
           and all pending text is displayed. *)
value print_newline : unit -> unit;;
        (* Equivalent to [print_flush] followed by a new line. *)

value print_if_newline : unit -> unit;;
        (* If the preceding line has not been split, the next formatting command
           is ignored. *)

(*** Tabulations *)
value open_tbox : unit -> unit;;
        (* Open a tabulation box. *)
value close_tbox : unit -> unit;;
        (* Close the most recently opened tabulation box. *)
value print_tbreak : int * int -> unit;;
        (* Break hint in a tabulation box.
           [print_tbreak (spaces, offset)] moves the insertion point to
           the next tabulation ([spaces] being added to this position).
           Nothing occurs if insertion point is already on a
           tabulation mark.
           If there is no next tabulation on the line, then a newline
           is printed and the insertion point moves to the first
           tabulation of the box.
           If a new line is printed, [offset] is added to the current
           indentation. *)
value set_tab : unit -> unit;;
        (* Set a tabulation mark at the current insertion point. *)
value print_tab : unit -> unit;;
        (* [print_tab ()] is equivalent to [print_tbreak (0,0)]. *)

(*** Margin *)
value set_margin : int -> unit;;
        (* [set_margin d] sets the value of the right margin
           to [d] (in characters): this value is used to detect line
           overflows that leads to split lines.
           Nothing happens if [d] is not greater than 1. *)
value get_margin : unit -> int;;
        (* Return the position of the right margin. *)

(*** Maximum indentation limit *)
value set_max_indent : int -> unit;;
        (* [set_max_indent d] sets the value of the maximum
           indentation limit to [d] (in characters):
           once this limit is reached, boxes are rejected to the left.
           Nothing happens if [d] is not greater than 1. *)
value get_max_indent : unit -> int;;
        (* Return the value of the maximum indentation limit (in
           characters). *)

(*** Formatting depth: maximum number of boxes allowed before ellipsis *)
value set_max_print_depth : int -> unit;;
        (* [set_max_print_depth max_depth] sets the maximum number
           of boxes simultaneously opened.
           Material inside boxes nested deeper is printed as an
           ellipsis (more precisely as the text returned by
           [get_ellipsis_text]).
           Nothing happens if [max_depth] is not greater than 1. *)
value get_max_print_depth : unit -> int;;
        (* Return the maximum number of boxes allowed before ellipsis. *)

(*** Ellipsis *)
value set_ellipsis_text : string -> unit;;
        (* Set the text of the ellipsis printed when too many boxes
           are opened (a single dot, [.], by default). *)
value get_ellipsis_text : unit -> string;;
        (* Return the the text of the ellipsis. *)

(*** Redirecting formatter output *)
value set_formatter_output : out_channel -> unit;;
        (* Redirect the pretty-printer output to the given channel. *)
value get_formatter_output : unit -> out_channel;;
        (* Return the channel connected to the pretty-printer. *)

