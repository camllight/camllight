(* A catch-all exception handler *)

#open "bool";;
#open "exc";;
#open "eq";;
#open "int";;
#open "fvect";;
#open "io";;
#open "obj";;
#open "sys";;

type qualid = {qual:string; id:string}
;;

let f fct arg =
  try
    fct arg
  with x ->
    flush std_out;
    begin match x with
      Out_of_memory ->
        prerr_string "Out of memory"
    | Match_failure(file, first_char, last_char) ->
        prerr_string "Pattern matching failed, file ";
        prerr_string file;
        prerr_string ", chars "; prerr_int first_char;
        prerr_char `-`; prerr_int last_char
    | x ->
        prerr_string "Uncaught exception: ";
        let tag = obj_tag (repr x) in
        begin try
          let ic = open_in_bin command_line.(0) in
          let pos_hdr = in_channel_length ic - 20 in
          seek_in ic pos_hdr;
          let size_code = input_binary_int ic in
          let size_data = input_binary_int ic in
          let size_symb = input_binary_int ic in
          let size_debug = input_binary_int ic in
          seek_in ic (pos_hdr - size_debug - size_symb);
          input_value ic;
          input_value ic;
          let tag_exn_table = (input_value ic : (qualid * int) vect) in
          close_in ic;
          if tag >= vect_length tag_exn_table then raise Exit;
          let (q,s) = tag_exn_table.(tag) in
          prerr_string q.qual;
          prerr_string "__";
          prerr_string q.id;
        with _ ->
          prerr_char `<`;
          prerr_int tag;
          prerr_char `>`
        end;
        if obj_size (repr x) > 0 then begin
          prerr_char `(`;
          for i = 0 to obj_size (repr x) - 1 do
            if i > 0 then prerr_string ", ";
            let arg = obj_field (repr x) i in
            if not (is_block arg) then
              prerr_int (magic_obj arg : int)
            else if obj_tag arg == 253 then begin
              prerr_char `"`;
              prerr_string (magic_obj arg : string);
              prerr_char `"`
            end else
              prerr_char `_`
          done;
          prerr_char `)`
        end
    end;
    prerr_char `\n`
;;
