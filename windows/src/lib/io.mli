(* Buffered input and output *)

type in_channel;;
type out_channel;;
        (* The abstract types of input channels and output channels. *)

exception End_of_file
        (* Raised when an operation cannot complete, because the end
           of the file has been reached. *)
;;
value stdin : in_channel
  and std_in : in_channel
  and stdout : out_channel
  and std_out : out_channel
  and stderr : out_channel
  and std_err : out_channel
        (* The standard input, standard output, and standard error output
           for the process. [std_in], [std_out] and [std_err] are respectively
	   synonymous with [stdin], [stdout] and [stderr]. *)
;;
value exit : int -> 'a
        (* Flush all pending writes on [std_out] and [std_err],
           and terminate the process, returning the given status code
	   to the operating system
           (usually 0 to indicate no errors, and a small positive integer
           to indicate failure.) This function should be called at
           the end of all standalone programs that output results on
           [std_out] or [std_err]; otherwise, the program may appear
           to produce no output, or its output may be truncated. *)
;;

(*** Output functions on standard output *)

value print_char : char -> unit
        (* Print the character on standard output. *)
  and print_string : string -> unit
        (* Print the string on standard output. *)
  and print_int : int -> unit
        (* Print the integer, in decimal, on standard output. *)
  and print_float : float -> unit
        (* Print the floating-point number, in decimal, on standard output. *)
  and print_endline : string -> unit
        (* Print the string, followed by a newline character, on
           standard output. *)
  and print_newline : unit -> unit
        (* Print a newline character on standard output, and flush
           standard output. This can be used to simulate line
           buffering of standard output. *)
;;

(*** Output functions on standard error *)

value prerr_char : char -> unit
        (* Print the character on standard error. *)
  and prerr_string : string -> unit
        (* Print the string on standard error. *)
  and prerr_int : int -> unit
        (* Print the integer, in decimal, on standard error. *)
  and prerr_float : float -> unit
        (* Print the floating-point number, in decimal, on standard error. *)
  and prerr_endline : string -> unit
        (* Print the string, followed by a newline character on standard error
	   and flush standard error. *)
;;

(*** Input functions on standard input *)

value read_line : unit -> string
        (* Flush standard output, then read characters from standard input
	   until a newline character is encountered. Return the string of
           all characters read, without the newline character at the end. *)
  and read_int : unit -> int
        (* Flush standard output, then read one line from standard input
           and convert it to an integer. Raise [Failure "int_of_string"]
           if the line read is not a valid representation of an integer. *)
  and read_float : unit -> float
        (* Flush standard output, then read one line from standard input
           and convert it to a floating-point number.
           The result is unspecified if the line read is not a valid
           representation of a floating-point number. *)
;;

(*** General output functions *)

value open_out : string -> out_channel
        (* Open the named file for writing, and return a new output channel
           on that file, positionned at the beginning of the file. The
           file is truncated to zero length if it already exists. It
           is created if it does not already exists.
           Raise [sys__Sys_error] if the file could not be opened. *)
  and open_out_bin : string -> out_channel
        (* Same as [open_out], but the file is opened in binary mode,
           so that no translation takes place during writes. On operating
           systems that do not distinguish between text mode and binary
           mode, this function behaves like [open_out]. *)
  and open_out_gen : sys__open_flag list -> int -> string -> out_channel
        (* [open_out_gen mode rights filename] opens the file named
           [filename] for writing, as above. The extra argument [mode]
           specify the opening mode (see [sys__open]). The extra
           argument [rights] specifies the file permissions, in case the
           file must be created (see [sys__open]).
           [open_out] and [open_out_bin] are special cases of this function. *)
  and open_descriptor_out : int -> out_channel = 1 "open_descriptor"
        (* [open_descriptor_out fd] returns a buffered output channel
           writing to the file descriptor [fd]. The file descriptor [fd]
           must have been previously opened for writing, else the behavior is
	   undefined. *)
  and flush : out_channel -> unit = 1 "flush"
        (* Flush the buffer associated with the given output channel, 
           performing all pending writes on that channel.
           Interactive programs must be careful about flushing [std_out]
           at the right times. *)
  and output_char : out_channel -> char -> unit = 2 "output_char"
        (* Write the character on the given output channel. *)
  and output_string : out_channel -> string -> unit
        (* Write the string on the given output channel. *)
  and output : out_channel -> string -> int -> int -> unit
        (* [output chan buff ofs len] writes [len] characters from string 
           [buff], starting at offset [ofs], to the output channel [chan].
           Raise [Invalid_argument "output"] if [ofs] and [len] do not
           designate a valid substring of [buff]. *)          
  and output_byte : out_channel -> int -> unit = 2 "output_char"
        (* Write one 8-bit integer (as the single character with that code)
           on the given output channel. The given integer is taken modulo
           256. *)
  and output_binary_int : out_channel -> int -> unit = 2 "output_int"
        (* Write one integer in binary format on the given output channel.
           The only reliable way to read it back is through the
           [input_binary_int] function. The format is compatible across
	   all machines for a given version of Caml Light. *)
  and output_value : out_channel -> 'a -> unit = 2 "extern_val"
        (* Write the representation of a structured value of any type
           to a channel. Circularities and sharing inside the value
           are detected and preserved. The object can be read back,
           by the function [input_value]. The format is compatible across
	   all machines for a given version of Caml Light. *)
  and seek_out : out_channel -> int -> unit = 2 "seek_out"
        (* [seek_out chan pos] sets the current writing position to [pos]
           for channel [chan]. This works only for regular files. On
           files of other kinds (such as terminals, pipes and sockets,)
	   the behavior is unspecified. *)
  and pos_out : out_channel -> int = 1 "pos_out"
        (* Return the current writing position for the given channel. *)
  and out_channel_length : out_channel -> int = 1 "channel_size"
        (* Return the total length (number of characters) of the
           given channel. *)
  and close_out : out_channel -> unit = 1 "close_out"
        (* Close the given channel, flushing all buffered write operations.
	   The behavior is unspecified if any of the above functions is
	   called on a closed channel. *)
;;

(*** General input functions *)

value open_in : string -> in_channel
        (* Open the named file for reading, and return a new input channel
           on that file, positionned at the beginning of the file.
           Raise [sys__Sys_error] if the file could not be opened. *)
  and open_in_bin : string -> in_channel
        (* Same as [open_in], but the file is opened in binary mode,
           so that no translation takes place during reads. On operating
           systems that do not distinguish between text mode and binary
           mode, this function behaves like [open_in]. *)
  and open_in_gen : sys__open_flag list -> int -> string -> in_channel
        (* [open_in_gen mode rights filename] opens the file named
           [filename] for reading, as above. The extra arguments
           [mode] and [rights] specify the opening mode and file permissions
           (see [sys__open]). [open_in] and [open_in_bin] are special cases
           of this function. *)
  and open_descriptor_in : int -> in_channel = 1 "open_descriptor"
        (* [open_descriptor_in fd] returns a buffered input channel
           reading from the file descriptor [fd]. The file descriptor [fd]
           must have been previously opened for reading, else the behavior is
	   undefined. *)
  and input_char : in_channel -> char = 1 "input_char"
        (* Read one character from the given input channel.
           Raise [End_of_file] if there are no more characters to read. *)
  and input_line : in_channel -> string = 1 "input_line"
        (* Read characters from the given input channel, until a
           newline character is encountered. Return the string of
           all characters read, without the newline character at the end.
           Raise [End_of_file] if the end of the file is reached
           before the line is complete. *)
  and input : in_channel -> string -> int -> int -> int
        (* [input chan buff ofs len] attempts to read [len] characters
           from channel [chan], storing them in string [buff], starting at
           character number [ofs]. It returns the actual number of characters
           read, between 0 and [len] (inclusive).
           A return value of 0 means that the end of file was reached.
           A return value between 0 and [len] exclusive means that
           no more characters were available at that time; [input] must be
           called again to read the remaining characters, if desired.
           Exception [Invalid_argument "input"] is raised if [ofs] and [len]
           do not designate a valid substring of [buff]. *)          
  and really_input : in_channel -> string -> int -> int -> unit
        (* [input chan buff ofs len] reads [len] characters
           from channel [chan], storing them in string [buff], starting at
           character number [ofs]. Raise [End_of_file] if
           the end of file is reached before [len] characters have been read.
           Raise [Invalid_argument "really_input"] if
           [ofs] and [len] do not designate a valid substring of [buff]. *)
  and input_byte : in_channel -> int = 1 "input_char"
        (* Same as [input_char], but return the 8-bit integer representing
           the character.
           Raise [End_of_file] if an end of file was reached. *)
  and input_binary_int : in_channel -> int = 1 "input_int"
        (* Read an integer encoded in binary format from the given input
           channel. See [output_binary_int].
           Raise [End_of_file] if an end of file was reached while reading the
	   integer. *)
  and input_value : in_channel -> 'a = 1 "intern_val"
        (* Read the representation of a structured value, as produced
           by [output_value], and return the corresponding value.
           This is not type-safe. The type of the returned object is
           not ['a] properly speaking: the returned object has one
           unique type, which cannot be determined at compile-time.
           The programmer should explicitly give the expected type of the
           returned value, using the following syntax:
                     [(input_value chan : type)].
	   The behavior is unspecified if the object in the file does not
	   belong to the given type. *)
  and seek_in : in_channel -> int -> unit = 2 "seek_in"
        (* [seek_in chan pos] sets the current reading position to [pos]
           for channel [chan]. *)
  and pos_in : in_channel -> int = 1 "pos_in"
        (* Return the current reading position for the given channel. *)
  and in_channel_length : in_channel -> int = 1 "channel_size"
        (* Return the total length (number of characters) of the
           given channel. This works only for regular files. On files of
           other kinds, the result is meaningless. *)
  and close_in : in_channel -> unit = 1 "close_in"
        (* Close the given channel. Anything can happen if any of the
           above functions is called on a closed channel. *)
;;

(*--*)

value fast_input : in_channel -> string -> int -> int -> int = 4 "input"
  and fast_really_input : in_channel -> string -> int -> int -> unit
  and fast_output : out_channel -> string -> int -> int -> unit = 4 "output"
;;
