(* librar.ml : builds a library by concatenating bytecode object files *)

#open "misc";;
#open "emit_phr";;

let offset_compiled_phrase ofs cp =
  { cph_pos = cp.cph_pos + ofs;
    cph_len = cp.cph_len;
    cph_reloc = cp.cph_reloc;
    cph_events = cp.cph_events;
    cph_pure = cp.cph_pure }
;;

let add_to_library outchan (offset, index_rest) filename =
  try
    let inchan = open_in_bin (find_in_path filename) in
    let ofs = input_binary_int inchan in
    let len = ofs - 4 in
    let buffer = create_string len in
    really_input inchan buffer 0 len;
    output outchan buffer 0 len;
    let old_index = (input_value inchan : compiled_phrase list) in
    close_in inchan;
    let new_index = map (offset_compiled_phrase offset) old_index in
      (offset + len, new_index @ index_rest)
  with Cannot_find_file name ->
    interntl__eprintf "Cannot find file %s.\n" name;
    raise Toplevel
;;    

let make_library file_list library_name =
  let outchan =
    open_out_bin library_name in
  try
    output_binary_int outchan 0;
    let (offset, index) =
      it_list (add_to_library outchan) (0, []) file_list in
    let pos_reloc =
      pos_out outchan in
    output_value outchan index;
    seek_out outchan 0;
    output_binary_int outchan pos_reloc;
    close_out outchan
  with x ->
    close_out outchan;
    remove_file library_name;
    raise x
;;

