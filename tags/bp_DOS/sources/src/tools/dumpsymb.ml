#open "sys";;
#open "const";;
#open "symtable";;

let print_qualid q =
  print_string q.qual; print_string "__"; print_string q.id
;;

let print_numtable f tbl =
  print_string "("; print_int tbl.num_cnt;
  print_string " entries)"; print_newline();
  hashtbl__do_table
    (fun qualid n ->
      f qualid; print_char ` `; print_int n; print_newline())
    tbl.num_tbl
;;

let print_exn (id, stamp) =
  print_qualid id; print_string "("; print_int stamp; print_string ")"
;;

let main() =
  let ic = open_in_bin command_line.(1) in
  let pos_hdr = in_channel_length ic - 20 in
  seek_in ic pos_hdr;
  let size_code = input_binary_int ic in
  let size_data = input_binary_int ic in
  let size_symb = input_binary_int ic in
  let size_debug = input_binary_int ic in
  seek_in ic (pos_hdr - size_debug - size_symb);
  let global_table = (input_value ic : qualified_ident numtable) in 
  let exn_tag_table = (input_value ic : (qualified_ident * int) numtable) in 
  let tag_exn_table = (input_value ic : (qualified_ident * int) vect) in 
  print_string "---- Table of globals:"; print_newline();
  print_numtable print_qualid global_table;
  print_newline();
  print_string "---- Table of exceptions:"; print_newline();
  print_numtable print_exn exn_tag_table;
  print_newline();
  print_string "---- Table of exception tags:"; print_newline();
  for i = 0 to vect_length tag_exn_table - 1 do
    print_int i; print_char `\t`;
    print_exn tag_exn_table.(i); print_newline()
  done
;;

printexc__f main ()
;;

  
