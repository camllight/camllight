#open "eq";;
#open "io";;
#open "exc";;
#open "obj";;
#open "fstring";;
#open "ref";;

type 'a current_value =
    Vcurr of 'a
  | Vundef
  | Veos
;;

type 'a stream =
    Sempty
  | Scons of 'a * 'a stream
  | Sapp of 'a stream * 'a stream
  | Sfunc of (unit -> 'a stream) * unit
  | Sgen of (unit -> 'a) * 'a current_value ref
;;

let rec stream_peek = function
    Sempty -> raise Parse_failure
  | Scons(x,_) -> x
  | Sapp(s1, s2) as s ->
      begin try
        stream_peek s1
      with Parse_failure ->
        update (repr s) (repr s2);
        stream_peek s
      end
  | Sfunc(f,_) as s ->
      update (repr s) (repr (f ()));
      stream_peek s
  | Sgen(prod,curr) as s ->
      match !curr with
        Vcurr x -> x
      | Veos -> raise Parse_failure
      | Vundef ->
          begin try
            let t = prod() in curr := Vcurr t; t
          with End_of_file | Parse_failure ->
            curr := Veos; raise Parse_failure
          end
;;

let rec stream_junk = function
    Scons(_, s') as s ->
      update (repr s) (repr s')
  | Sapp(s1,_) ->
      stream_junk s1
  | Sgen(prod,curr) as s ->
      curr := Vundef
  | _ ->
      ()
;;

let stream_require strm =
  try stream_peek strm with Parse_failure -> raise Parse_error
;;

let parser_require x (strm : 'a stream) =
  try x strm with Parse_failure -> raise Parse_error
;;

(* Other useful functions *)

let stream_next s =
  let x = stream_peek s in stream_junk s; x
  (* Don't define stream_next = function [<'x>] -> x because this causes
     a problem with type stamps while compiling stream.ml. *)
;;

let stream_from rf =
   Sgen(rf, ref Vundef)
;;

let stream_of_string s =
  let i = ref (-1) in
  stream_from
    (fun () ->
      incr i;
      if !i >= string_length s then raise Parse_failure else nth_char s !i)
;;

let stream_of_channel ic =
  stream_from (fun () -> input_char ic)
;;

let do_stream f strm =
  let rec do_rec() =
    f(stream_peek strm); stream_junk strm; do_rec() in
  try
    do_rec()
  with Parse_failure -> ()
;;

let stream_check p strm =
  let x = stream_peek strm in
  if p x then (stream_junk strm; x) else raise Parse_failure
;;

let end_of_stream strm =
  if try let _ = stream_peek strm in false with Parse_failure -> true
  then ()
  else raise Parse_failure
;;

let rec stream_get = function
    Sempty -> raise Parse_failure
  | Scons(x,s) -> (x,s)
  | Sapp(s1, s2) as s ->
      let (x,s') as r =
        try
          let (x,s') = stream_get s1 in (x, Sapp(s',s2))
        with Parse_failure ->
          stream_get s2 in
      update (repr s) (repr (Scons(x,s')));
      r
  | Sfunc(f,_) as s ->
      update (repr s) (repr (f()));
      stream_get s
  | Sgen(prod, curr) as s ->
      match !curr with
        Vcurr x ->
          let s' = Sgen(prod, ref Vundef) in
          update (repr s) (repr (Scons (x, s')));
          (x, s')
      | Vundef ->
          begin try
            let t = prod() in
            let s' = Sgen(prod, ref Vundef) in
            update (repr s) (repr (Scons (t, s')));
            (t, s')
          with End_of_file | Parse_failure ->
            update (repr s) (repr Sempty);
            raise Parse_failure
          end
      | Veos ->
         raise Parse_failure
;;
