#open "unix";;
#open "printf";;


let toplevel_bin = ref "camllight"
;;


(* To be used in conjunction with add_fileinput.
    Asynchronous fill of a buffer from a fd
*)



let async_read fd =
  let buflen = 512 in
  let buffer = create_string buflen
  in
  fun () ->
  let n = read fd buffer 0 buflen in
     sub_string buffer 0 n
;;

(* fully write a string *)
let rec full_write fd str =
  let len = string_length str in
  let rec wr lenleft pos =
    if lenleft = 0 then ()
    else let written = write fd str pos lenleft in
         wr (len - written) (pos + written) 
  in
    wr len 0
;;


let slave_toplevel () =
  let (slavein,masterout) = pipe() 
  and (masterin,slaveout) = pipe () in
  match (fork()) with
    0 -> (* The slave toplevel *)
      close masterin;
      close masterout;
      dup2 slavein stdin;
      dup2 slaveout stdout;
      dup2 slaveout stderr;
      execvp !toplevel_bin [| "slave Caml Light" |];
      failwith "execvp failed"
  | n -> (* The parent *)
      close slavein;
      close slaveout;
      masterin, async_read masterin,  full_write masterout, n
;;


