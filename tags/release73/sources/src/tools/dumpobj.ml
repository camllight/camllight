(* To show the contents of a .zo file.*)

(*
camlc -g -o dumpobj -I ../lib -I ../compiler -I ../linker printexc.zo opcodes.zo dumpobj.ml
*)

#open "const";;
#open "reloc";;
#open "emit_phr";;
#open "opcodes";;
#open "opnames";;

let input_u16 ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
    b1 + lshift_left b2 8
;;

let input_s16 ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let n = b1 + lshift_left b2 8 in
  if n >= 32768 then n - 65536 else n
;;

let input_s32 ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  let b4 = input_byte ic in
    b1 + lshift_left b2 8 + lshift_left b3 16 + lshift_left b4 24
;;

let print_code ic len =
  let start = pos_in ic in
  let print_depl ic =
    let orig = pos_in ic - start in
      print_int (orig + input_s16 ic) in
  let stop = start + len in
    while pos_in ic < stop do
      print_int (pos_in ic - start); print_string "\t";
      let op = input_byte ic in
        if op >= vect_length names_of_instructions then
          print_string "??? "
        else begin
          print_string names_of_instructions.(op); print_string " "
        end;
        if op == ACCESS || op == DUMMY || op == ENDLET
        || op == CONSTBYTE || op == ATOM || op == GETFIELD || op == SETFIELD
        || op == MAKEBLOCK1 || op == MAKEBLOCK2 || op == MAKEBLOCK3
        || op == MAKEBLOCK4 then
          print_int(input_byte ic)
        else if op == GETGLOBAL || op == SETGLOBAL
        || op == PUSH_GETGLOBAL_APPLY || op == PUSH_GETGLOBAL_APPTERM
        || op == C_CALL1 || op == C_CALL2 || op == C_CALL3
        || op == C_CALL4 || op == C_CALL5 then
          print_int(input_u16 ic)
        else if op == CONSTSHORT then
          print_int(input_s16 ic)
        else if op == MAKEBLOCK then
          print_int(input_s32 ic)
        else if op == CUR || op == LETREC1 || op == PUSHTRAP
        || op == BRANCH || op == BRANCHIF || op == BRANCHIFNOT
        || op == POPBRANCHIFNOT || op == BRANCHIFEQ || op == BRANCHIFNEQ
        || op == BRANCHIFLT || op == BRANCHIFGT || op == BRANCHIFLE
        || op == BRANCHIFGE then
          print_depl ic
        else if op == FLOATOP then
          print_string names_of_float_instructions.(input_byte ic)
        else if op == SWITCH then
          (let n = input_byte ic in
           let orig = pos_in ic - start in
             for i = 0 to n-1 do
               print_int (orig + input_s16 ic); print_string ", "
             done)
        else if op == BRANCHINTERVAL then
          (print_depl ic; print_string ", "; print_depl ic)
        else if op == C_CALLN then
          (print_int(input_byte ic);
           print_string ", ";
           print_int(input_u16 ic))
        else
          ();
        print_newline()
    done
;;

let print_global g =
  print_string g.qual; print_string "__"; print_endline g.id
;;

let print_reloc (info, pos) =
  print_string "\t"; print_int pos; print_string "\t";
  match info with
    Reloc_literal _ -> print_endline "const"
  | Reloc_getglobal g -> print_string "require\t"; print_global g
  | Reloc_setglobal g -> print_string "provide\t"; print_global g
  | Reloc_tag(g,s) -> print_string "exc.tag\t"; print_global g
  | Reloc_primitive s -> print_string "prim\t"; print_endline s
;;

let print_entry ic phr =
  print_string "Offset ";
  print_int phr.cph_pos;
  print_string ", length ";
  print_int phr.cph_len;
  if phr.cph_pure then print_endline ", pure" else print_endline ", impure";
  seek_in ic phr.cph_pos;
  print_code ic phr.cph_len;
  do_list print_reloc phr.cph_reloc
;;

let dump filename =
  print_string "File "; print_endline filename;
  let ic = open_in_bin filename in
  let n = input_binary_int ic in
  seek_in ic n;
  let index = (input_value ic : compiled_phrase list) in
  do_list (print_entry ic) (rev index);
  close_in ic
;;

let main() =
  for i = 1 to vect_length sys__command_line - 1 do
    dump sys__command_line.(i)
  done;
  exit 0
;;

printexc__f main ();;
