(* To buffer bytecode during emission *)

let out_buffer = ref (create_string 64)
and out_position = ref 0
;;

let realloc_out_buffer () =
  let new_buffer = create_string (2 * string_length !out_buffer) in
    replace_string new_buffer !out_buffer 0;
    out_buffer := new_buffer;
    ()
;;

let init_out_code () =
  out_position := 0;
  ()
;;

let out b =
  if !out_position >= string_length !out_buffer then realloc_out_buffer();
  set_nth_char !out_buffer !out_position (fchar__char_of_int b);
  incr out_position
;;

let out_short s =
  if s >= 32768 || s < -32768 then
    error__displacement_overflow ()
  else begin
    out s; out (lshift_right s 8)
  end
;;

