(* Input-output *)

#open "eq";;
#open "exc";;
#open "int";;
#open "sys";;
#open "fstring";;
#open "ref";;

type in_channel
 and out_channel
;;

let std_in = open_descriptor_in 0
and std_out = open_descriptor_out 1
and std_err = open_descriptor_out 2
;;

let stdin = std_in
and stdout = std_out
and stderr = std_err
;;

let exit n =
  flush std_out;
  flush std_err;
  sys__exit n
;;

let open_in_gen mode rights filename =
  open_descriptor_in (open filename mode rights)
;;

let open_in = open_in_gen [O_RDONLY; O_TEXT] 0
and open_in_bin = open_in_gen [O_RDONLY; O_BINARY] 0
;;

let input chan buff ofs len =
  if len < 0 or ofs < 0 or ofs+len > string_length buff then
    invalid_arg "input"
  else
    fast_input chan buff ofs len
;;

let rec fast_really_input chan buff ofs len =
  if len <= 0 then () else
    match fast_input chan buff ofs len with
      0 -> raise End_of_file
    | r -> fast_really_input chan buff (ofs+r) (len-r)
;;

let really_input chan buff ofs len =
  if len < 0 or ofs < 0 or ofs+len > string_length buff then
    invalid_arg "really_input"
  else
    fast_really_input chan buff ofs len
;;

let read_line () = flush std_out; input_line std_in
;;
let read_int () = int__int_of_string (read_line())
;;
let read_float () = float__float_of_string (read_line())
;;

let open_out_gen mode rights filename =
  open_descriptor_out(open filename mode rights)
;;

let open_out =
  open_out_gen [O_WRONLY; O_TRUNC; O_CREAT; O_TEXT] (s_irall + s_iwall)
and open_out_bin =
  open_out_gen [O_WRONLY; O_TRUNC; O_CREAT; O_BINARY] (s_irall + s_iwall)
;;

let output chan buff ofs len =
  if len < 0 or ofs < 0 or ofs+len > string_length buff then
    invalid_arg "output"
  else
    fast_output chan buff ofs len
;;

let output_string channel s =
  fast_output channel s 0 (string_length s)
;;

let print_char =
  output_char std_out
;;
let print_string =
  output_string std_out
;;
let print_int i =
  print_string (int__string_of_int i)
;;
let print_float f =
  print_string (float__string_of_float f)
;;
let print_endline s =
  print_string s;
  print_char `\n`
;;

let print_newline () =
  print_char `\n`;
  flush std_out
;;

let prerr_char =
  output_char std_err
;;
let prerr_string =
  output_string std_err
;;
let prerr_int i =
  prerr_string (int__string_of_int i)
;;
let prerr_float f =
  prerr_string (float__string_of_float f)
;;
let prerr_endline s =
  prerr_string s;
  prerr_char `\n`;
  flush std_err
;;

