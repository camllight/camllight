(* Handlings of local labels and backpatching *)

#open "misc";;
#open "instruct";;
#open "buffcode";;

type label_definition =
    Label_defined of int
  | Label_undefined of (int * int) list
;;

let label_table  = ref ([| |] : label_definition vect)
;;

let reset_label_table () =
  label_table := (make_vect 16 (Label_undefined [])); ()
;;

let extend_label_table needed =
  let old = vect_length !label_table in
  let new_table = make_vect ((needed / old + 1) * old) (Label_undefined []) in
  for i = 0 to pred old do
    new_table.(i) <- (!label_table).(i)
  done;
  label_table := new_table; ()
;;

let define_label lbl =
  if lbl >= vect_length !label_table then extend_label_table lbl;
  match (!label_table).(lbl) with
    Label_defined _ ->
      fatal_error "define_label : already defined"
  | Label_undefined L ->
      let currpos = !out_position in
        (!label_table).(lbl) <- (Label_defined currpos);
        match L with
            [] -> ()
          |  _ -> do_list (fun (pos,orig) -> out_position := pos;
                                             out_short (currpos - orig)) L;
                  out_position := currpos
;;

let out_label_with_orig orig lbl =
  if lbl == Nolabel then fatal_error "out_label: undefined label";
  if lbl >= vect_length !label_table then extend_label_table lbl;
  match (!label_table).(lbl) with
    Label_defined def ->
      out_short (def - orig)
  | Label_undefined L ->
      (!label_table).(lbl) <-
        Label_undefined((!out_position, orig) :: L);
      out_short 0
;;

let out_label l = out_label_with_orig !out_position l
;;



