#open "sys";;
#open "unix";;

let do_dir f dirname =
  let d = opendir dirname in
  try
    while true do
      f(readdir d)
    done
  with End_of_file ->
    closedir d
;;

let buffer_size = 8192;;
let buffer = create_string buffer_size;;

let file_copy input_name output_name =
  let fd_in = open input_name [O_RDONLY] 0 in
  let fd_out = open output_name [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in
  let rec copy_loop () =
    match read fd_in buffer 0 buffer_size with
      0 -> ()
    | r -> write fd_out buffer 0 r; copy_loop () in
  copy_loop ();
  close fd_in;
  close fd_out;;

let set_infos filename infos =
  utimes filename infos.st_atime infos.st_mtime;
  chmod filename infos.st_perm;
  try
    chown filename infos.st_uid infos.st_gid
  with Unix_error(EPERM,_,_) ->
    ()
;;

let rec copy source dest =
  let infos = lstat source in
  match infos.st_kind with
    S_REG ->
      file_copy source dest;
      set_infos dest infos
  | S_LNK ->
      let link = readlink source in
      symlink link dest
  | S_DIR ->
      mkdir dest 0o700;
      do_dir
        (fun file ->
          if file = "." || file = ".." then () else
            copy (source ^ "/" ^ file) (dest ^ "/" ^ file))
        source;
      set_infos dest infos
  | _ ->
      prerr_endline ("Can't cope with special file " ^ source)
;;

let main () =
  if vect_length command_line <> 3 then begin
    prerr_endline "Usage: copy_rec <source> <destination>";
    io__exit 2
  end else begin
    copy command_line.(1) command_line.(2);
    io__exit 0
  end
;;
handle_unix_error main ();;
