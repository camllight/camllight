(* To hide the global variables internal to the toplevel system. *)

#open "const";;
#open "sys";;
#open "symtable";;

let (debug_flag, input_name, output_name, perv_pos) =
  if command_line.(1) = "-g"
  then (true, command_line.(2), command_line.(3), 4)
  else (false, command_line.(1), command_line.(2), 3)
;;

let pervasives =
  map filename__basename
      (list_of_vect(sub_vect command_line perv_pos
                             (vect_length command_line - perv_pos)))
;;

let qualid_pervasive qualid =
  mem qualid.qual pervasives
;;
  
let expunge_numtable is_pervasive size nt =
  let new_tbl = hashtbl__new size in
    hashtbl__do_table
      (fun key info ->
        if is_pervasive key then hashtbl__add new_tbl key info)
      nt.num_tbl;
    { num_cnt = nt.num_cnt; num_tbl = new_tbl }
;;

let expunge_vect v =
  for i = 0 to vect_length v - 1 do
    let (q,s) = v.(i) in
      if qualid_pervasive q then () else
        v.(i) <- ({qual="<internal>";id="<exc>"}, 0)
  done
;;

let copy_bytes inchan outchan n =
  let copy_buffer =
    create_string 4096 in
  let rec copy n =
    if n <= 0 then () else begin
      match input inchan copy_buffer 0 (min n (string_length copy_buffer)) with
        0 -> failwith "truncated input file"
      | r -> output outchan copy_buffer 0 r; copy (n-r)
    end in
  copy n
;;

let main() =
  let ic = open_in_bin input_name
  and oc = open_out_gen [O_WRONLY; O_TRUNC; O_CREAT; O_BINARY]
                        (s_irall + s_iwall + s_ixall)
                        output_name in
  let pos_hdr = in_channel_length ic - 20 in
  seek_in ic pos_hdr;
  let size_code = input_binary_int ic in
  let size_data = input_binary_int ic in
  let size_symb = input_binary_int ic in
  let size_debug = input_binary_int ic in
  seek_in ic 0;
  copy_bytes ic oc (pos_hdr - size_symb - size_debug);
  let global_table = (input_value ic : qualified_ident numtable) in 
  let exn_tag_table = (input_value ic : (qualified_ident * int) numtable) in 
  let tag_exn_table = (input_value ic : (qualified_ident * int) vect) in 
  let new_global_table =
    expunge_numtable qualid_pervasive 263 global_table in
  let new_exn_tag_table =
    expunge_numtable (fun (q,s) -> qualid_pervasive q) 31 exn_tag_table in
  expunge_vect tag_exn_table;
  let pos1 = pos_out oc in
  output_value oc new_global_table;
  output_value oc new_exn_tag_table;
  output_value oc tag_exn_table;
  let pos2 = pos_out oc in
  if debug_flag then copy_bytes ic oc size_debug;
  let pos3 = pos_out oc in
  close_in ic;
  output_binary_int oc size_code;
  output_binary_int oc size_data;
  output_binary_int oc (pos2 - pos1);
  output_binary_int oc (pos3 - pos2);
  output_string oc "CL07";
  close_out oc
;;

printexc__f main ()
;;
