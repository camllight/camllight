(* Formatting printing functions *)

#open "io";;

value fprintf: out_channel -> string -> 'a
        (* [fprintf outchan format arg1 ... argN] formats the arguments
           [arg1] to [argN] according to the format string [format],
           and outputs the resulting string on the channel [outchan].
           The format is a character string which contains two types of
           objects:  plain  characters, which are simply copied to the
           output channel, and conversion specifications, each of which
           causes  conversion and printing of one argument.
           Conversion specifications consist in the [%] character, followed
           by optional flags and field widths, followed by one conversion
           character. The conversion characters and their meanings are:
-          [d] or [i]: convert an integer argument to signed decimal
-          [u]: convert an integer argument to unsigned decimal
-          [x]: convert an integer argument to unsigned hexadecimal,
                using lowercase letters.
-          [X]: convert an integer argument to unsigned hexadecimal,
                using uppercase letters.
-          [s]: insert a string argument
-          [c]: insert a character argument
-          [f]: convert a floating-point argument to decimal notation,
                in the style [dddd.ddd]
-          [e] or [E]: convert a floating-point argument to decimal notation,
                in the style [d.ddd e+-dd] (mantissa and exponent)
-          [g] or [G]: convert a floating-point argument to decimal notation,
                in style [f] or [e], [E] (whichever is more compact)
-          [b]: convert a boolean argument to the string [true] or [false]
-          Refer to the C library [printf] function for the meaning of
           flags and field width specifiers.
           The exception [Invalid_argument] is raised if the types of the
           provided arguments do not match the format. The exception is
           also raised if too many arguments are provided. If too few
           arguments are provided, printing stops just before converting
           the first missing argument. *)

  and printf: string -> 'a
        (* Same as [fprintf], but output on [std_out]. *)

  and fprint: out_channel -> string -> unit
        (* Print the given string on the given output channel, without
           any formatting. *)

  and print: string -> unit
        (* Print the given string on [std_out], without any formatting.
	   This is the same function as [print_string] of module [io]. *)
;;
