(* Pretty print *)

(* This module implements a pretty-printing facility to format text
   within boxes; the pretty-printer breaks lines using specified break
   hints. *) 

#open "io";;

(* Boxes *)
value open_vbox : int -> unit;;
        (* [open_vbox my_number] opens a new pretty-printing box
           with offset [my_number]. 
           This box is ``vertical'': every break hint inside this
           box leads to a new line.
           Every new line in the box is indented [my_number] more than the
           indentation of the box. *)
value open_hbox : unit -> unit;;
        (* [open_hbox ()] opens a new pretty-printing box.
           This box is ``horizontal'': no new line occurs in
           this box. *) 
value open_hvbox : int -> unit;;
        (* [open_hovbox my_number] opens a new pretty-printing box
           with offset [my_number]. 
           This box is ``horizontal-vertical'': it behaves as an
           ``horizontal'' box if it fits on a single line,
           otherwise it behaves as a ``vertical'' one.
           Every new line in the box is indented [my_number] more than the
           indentation of the box. *)
value open_hovbox : int -> unit;;
        (* [open_hovbox my_number] opens a new pretty-printing box
           with offset [my_number]. 
           This box is ``horizontal or vertical'': break hints
           inside this box may lead to a new line, if there is no more room
           on the line to print the rest of the box.
           Every new line in the box is indented [my_number] more than the
           indentation of the box. *)
value close_box : unit -> unit;;
        (* Close the most recently opened pretty-printing box. *)

(* Formatting functions *)
value print_as : int -> string -> unit;;
        (* [print_as my_number my_string] prints [my_string] into the
           current box. The pretty-printer formats [my_string] as if
           it were of length [my_number]. *)
value print_string : string -> unit;;
        (* [print_string my_string] is equivalent to
           [print_as (string_length my_string) my_string]. *)
value print_int : int -> unit;;
        (* Print an integer. *)
value print_float : float -> unit;;
        (* Print a floating point number. *)
value print_char : char -> unit;;
        (* Print a character. *)
value print_bool : bool -> unit;;
        (* Print an boolean. *)

(* Breaking hints *)
value print_break : int * int -> unit;;
        (* Break hint in a pretty-printing box.
           [print_break (spaces, offset)] indicates that the line may
           be broken at this point, if the pretty-printer cannot
           display the content of the current box on the line.
           If a new line occurs, [offset] is added to
           the current indentation, otherwise [spaces] spaces are printed. *)
value print_cut : unit -> unit;;
        (* [print_cut ()] is equivalent to [print_break (0,0)]. *)
value print_space : unit -> unit;;
        (* [print_space ()] is equivalent to [print_break (1,0)]. *)
value force_newline : unit -> unit;;
        (* Force a new line in the current box. *)

value print_flush : unit -> unit;;
        (* Flush the pretty printer: all opened boxes are closed,
           and all pending text is displayed. *)
value print_newline : unit -> unit;;
        (* Equivalent to [print_flush] followed by a new line. *)

value print_if_newline : unit -> unit;;
        (* If the current line is not a new one, the next formating command
           is ignored. *)
(* Tabulation boxes *)
value open_tbox : unit -> unit;;
        (* Open a tabulation box. *)
value close_tbox : unit -> unit;;
        (* Close the most recently opened tabulation box. *)

(* Tabulation breaks *)
value print_tbreak : int * int -> unit;;
        (* Break hint in a tabulation box.
           [print_tbreak (spaces, offset)] moves insertion point to
           the next tabulation ([spaces] being added to this position).
           If there is no next tabulation on the line then a new line
           occurs and insertion point moves to the first tabulation of
           the box.
           If a new line occurs [offset] is added to the current
           indentation. *)
value set_tab : unit -> unit;;
        (* Set a tabulation after the last displayed character. *)
value print_tab : unit -> unit;;
        (* [print_tab ()] is equivalent to [print_tbreak (0,0)]. *)

(* Margin *)
value set_margin : int -> unit;;
        (* Set the value of the right margin (in characters). *)
value get_margin : unit -> int;;
        (* Return the value of the right margin. *)

(* Maximum indentation limit *)
value set_max_indent : int -> unit;;
        (* Set the value of the maximum indentation limit (in characters):
           boxes opened further are rejected to the left. *)
value get_max_indent : unit -> int;;
        (* Return the value of the maximum indentation limit (in
           characters). *)

(* Formatting depth: maximum number of boxes allowed
   before ellipsis mechanism (get_max_print_depth) *)
value set_max_print_depth : int -> unit;;
        (* [set_max_print_depth my_number] sets the maximum number
           of boxes simultaneously opened.
           Material inside boxes nested deeper is printed as an
           ellipsis (more precisely the text returned by
           [get_ellipsis_text]). *)
value get_max_print_depth : unit -> int;;
        (* Return the maximum number of boxes allowed before ellipsis. *)

(* Ellipsis *)
value set_ellipsis_text : string -> unit;;
        (* Set the text of the ellipsis printed when too many boxes
           are opened. *)
value get_ellipsis_text : unit -> string;;
        (* Return the the text of the ellipsis. *)

(* Redirecting formatter output *)
value set_formatter_output : out_channel -> unit;;
        (* Redirect the pretty-printer output to the given channel. *)
value get_formatter_output : unit -> out_channel;;
        (* Return the channel connected to the pretty-printer. *)

(*--*)
