(* Formatting printing functions *)

#open "io";;

type ('a, 'b, 'c) format;;
        (* The type of format strings. ['a] is the type of the parameters
           of the string, ['c] is the result type for the [printf]-style
           function, and ['b] is the type of the first argument given to
           [%a] and [%t] printing functions. *)

value fprintf: out_channel -> ('a, out_channel, unit) format -> 'a
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
-          [a]: user-defined printer. Takes two arguments and apply the first
                one to [outchan] (the current output channel) and to the second
                argument. The first argument must therefore have type
                [out_channel -> 'b -> unit] and the second ['b].
                The output produced by the function is therefore inserted
                in the output of [fprintf] at the current point.
-          [t]: same as [%a], but takes only one argument (with type
                [out_channel -> unit]) and apply it to [outchan].
-          Refer to the C library [printf] function for the meaning of
           flags and field width specifiers.

           If too few arguments are provided, printing stops just
           before converting the first missing argument. *)

  and printf: ('a, out_channel, unit) format -> 'a
        (* Same as [fprintf], but output on [std_out]. *)

  and eprintf: ('a, out_channel, unit) format -> 'a
        (* Same as [fprintf], but output on [std_err]. *)

  and sprintf: ('a, unit, string) format -> 'a
        (* Same as [fprintf], except that the result of the formatting
           is returned as a string instead of being written on a channel. *)

  and fprint: out_channel -> string -> unit
        (* Print the given string on the given output channel, without
           any formatting. This is the same function as [output_string]
           of module [io]. *)

  and print: string -> unit
        (* Print the given string on [std_out], without any formatting.
	   This is the same function as [print_string] of module [io]. *)

  and eprint: string -> unit
        (* Print the given string on [std_err], without any formatting.
	   This is the same function as [prerr_string] of module [io]. *)

;;
