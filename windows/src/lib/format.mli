(* Pretty print *)

(* This module implements a pretty-printing facility to format text
   within ``pretty-printing boxes''; the pretty-printer breaks lines
   using specified break hints. The behaviour of pretty-printing
   commands is unspecified if there is no opened pretty-printing box. *)

#open "io";;

(* Boxes *)
value open_vbox : int -> unit;;
        (* [open_vbox d] opens a new pretty-printing box
           with offset [d]. 
           This box is ``vertical'': every break hint inside this
           box leads to a new line.
<<<<<<< format.mli
           Every new line in this box is indented [my_number]
           more than the indentation of the box. *)
=======
           Every new line in the box is indented [d] more than the
           indentation of the box. *)
>>>>>>> 1.2
value open_hbox : unit -> unit;;
        (* [open_hbox ()] opens a new pretty-printing box.
           This box is ``horizontal'': no new line occurs in this box
           (new lines may occur inside boxes nested deeper). *)
value open_hvbox : int -> unit;;
        (* [open_hovbox d] opens a new pretty-printing box
           with offset [d]. 
           This box is ``horizontal-vertical'': it behaves as an
           ``horizontal'' box if it fits on a single line,
           otherwise it behaves as a ``vertical'' one.
<<<<<<< format.mli
           Every new line in this box is indented [my_number] more than the
=======
           Every new line in the box is indented [d] more than the
>>>>>>> 1.2
           indentation of the box. *)
value open_hovbox : int -> unit;;
        (* [open_hovbox d] opens a new pretty-printing box
           with offset [d]. 
           This box is ``horizontal or vertical'': break hints
           inside this box may lead to a new line, if there is no more room
           on the line to print the rest of the box.
<<<<<<< format.mli
           Every new line in this box is indented [my_number] more than the
=======
           Every new line in the box is indented [d] more than the
>>>>>>> 1.2
           indentation of the box. *)
value close_box : unit -> unit;;
        (* Close the most recently opened pretty-printing box. *)

(* Formatting functions *)
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

(* Breaking hints *)
value print_break : int * int -> unit;;
<<<<<<< format.mli
        (* Indicate a break hint in the current pretty-printing box.
           [print_break (spaces, offset)] indicates that the line may
           be broken at this point, if the pretty-printer cannot
           display the content of the current box on the line.
           If a new line occurs, [offset] is added to
           the current indentation, otherwise [spaces] spaces are printed. *)
=======
        (* Break hint in a pretty-printing box.
           [print_break (nspaces, offset)] indicates that the line may
           be split (a newline character is printed) at this point,
           if the contents of the current box do not fit on one line.
           If the line is split at that point, [offset] is added to
           the current indentation, otherwise [nspaces] spaces are printed. *)
>>>>>>> 1.2
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
        (* If the current line is not a new one, the next formatting command
           is ignored. *)

(* Tabulation boxes *)
value open_tbox : unit -> unit;;
        (* Open a tabulation box. *)
value close_tbox : unit -> unit;;
        (* Close the most recently opened tabulation box. *)

(* Tabulation breaks *)
value print_tbreak : int * int -> unit;;
<<<<<<< format.mli
        (* Indicates a break hint in the current tabulation box.
           [print_tbreak (spaces, offset)] moves insertion point to
=======
        (* Break hint in a tabulation box.
           [print_tbreak (spaces, offset)] moves the insertion point to
>>>>>>> 1.2
           the next tabulation ([spaces] being added to this position).
<<<<<<< format.mli
           Nothing occurs if insertion point is already on a
           tabulation mark.
           If there is no next tabulation on the line then a new line
           occurs and insertion point moves to the left-most tabulation of
           the box.
           If a new line occurs [offset] is added to the current
=======
           If there is no next tabulation on the line, then a newline
           is printed and the insertion point moves to
           the first tabulation of the box.
           If a newline is printed, [offset] is added to the current
>>>>>>> 1.2
           indentation. *)
value set_tab : unit -> unit;;
        (* Set a tabulation mark at the current insertion point. *)
value print_tab : unit -> unit;;
        (* [print_tab ()] is equivalent to [print_tbreak (0,0)]. *)

(* Margin *)
value set_margin : int -> unit;;
<<<<<<< format.mli
        (* [set_margin my_number] sets the value of the right margin
           to [my_number] (count in characters): this
           value guides the detection of line overflows that leads to
           line break decisions.
           Nothing happens if [my_number] is not greater than 1. *)
=======
        (* Set the position of the right margin (in characters). *)
>>>>>>> 1.2
value get_margin : unit -> int;;
        (* Return the position of the right margin. *)

(* Maximum indentation limit *)
value set_max_indent : int -> unit;;
<<<<<<< format.mli
        (* [set_max_indent my_number] sets the value of the maximum
           indentation limit to [my_number] (in characters):
           boxes opened further are rejected to the left.
           Nothing happens if [my_number] is not greater than 1. *)
=======
        (* Set the value of the maximum indentation limit (in characters):
           once this limit is reached, boxes are rejected to the left. *)
>>>>>>> 1.2
value get_max_indent : unit -> int;;
        (* Return the value of the maximum indentation limit (in
           characters). *)

(* Formatting depth: maximum number of boxes allowed before ellipsis *)
value set_max_print_depth : int -> unit;;
        (* [set_max_print_depth max_depth] sets the maximum number
           of boxes simultaneously opened.
           Material inside boxes nested deeper is printed as an
           ellipsis (more precisely the text returned by
           [get_ellipsis_text]).
           Nothing happens if [my_number] is not greater than 1. *)
value get_max_print_depth : unit -> int;;
        (* Return the maximum number of boxes allowed before ellipsis. *)

(* Ellipsis *)
value set_ellipsis_text : string -> unit;;
        (* Set the text of the ellipsis printed when too many boxes
           are opened (by default: [...]). *)
value get_ellipsis_text : unit -> string;;
        (* Return the the text of the ellipsis. *)

(* Redirecting formatter output *)
value set_formatter_output : out_channel -> unit;;
        (* Redirect the pretty-printer output to the given channel. *)
value get_formatter_output : unit -> out_channel;;
        (* Return the channel connected to the pretty-printer. *)

