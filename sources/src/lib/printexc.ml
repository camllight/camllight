(* A catch-all exception handler *)

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
        prerr_string "-"; prerr_int last_char
    | Failure s ->
        prerr_string "Evaluation failed : "; prerr_string s
    | Invalid_argument s ->
        prerr_string "Invalid argument : "; prerr_string s
    | Sys_error msg ->
        prerr_string "System call failed : ";
        prerr_string msg
    | x ->
        let tag = obj_tag (repr x) in
          prerr_string "Uncaught exception ";
          prerr_string (string_of_int tag);
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
                if tag >= vect_length tag_exn_table then
                  prerr_string " (never compiled)"
                else begin
                  let (q,s) = tag_exn_table.(tag) in
                  prerr_string " (";
                  prerr_string q.qual;
                  prerr_string "__";
                  prerr_string q.id;
                  prerr_string ")"
                end;
            close_in ic
          with _ ->
            ()
          end
    end;
    prerr_char `\n`;
    io__exit 2
;;
