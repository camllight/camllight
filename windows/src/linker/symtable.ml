#open "misc";;
#open "const";;
#open "predef";;
#open "printf";;

(* symtable.ml : to assign numbers to global variables and so on *)

let object_name = ref "";;

(* Hashtables for numbering objects *)

type 'a numtable =
  { mutable num_cnt: int;                   (* The current number *)
    mutable num_tbl: ('a, int) hashtbl__t } (* The table *)
;;

let new_numtable size =
  { num_cnt = 0; num_tbl = hashtbl__new size }
;;

let find_in_numtable nt =
  hashtbl__find nt.num_tbl

and enter_in_numtable nt key =
  let c = nt.num_cnt in
    nt.num_cnt <- succ nt.num_cnt;
    hashtbl__add nt.num_tbl key c;
    c

and remove_from_numtable nt key =
  hashtbl__remove nt.num_tbl key
;;

let reserve_in_numtable nt key =
  let _ = enter_in_numtable nt key in ();;

(* Global variables *)

let global_table =
  ref (new_numtable 1 : qualified_ident numtable)
and literal_table =
  ref ([] : (int * struct_constant) list)
;;

let get_slot_for_variable qualid =
  try
    find_in_numtable !global_table qualid
  with Not_found ->
    if string_length !object_name > 0 then
      interntl__eprintf
        "The global value %s__%s is referenced (from %s) \
         before being defined.\n\
         Please link %s.zo before %s.\n"
        qualid.qual qualid.id !object_name qualid.qual !object_name
    else
      interntl__eprintf
        "The global value %s__%s is referenced before being defined.\n\
         Please load an implementation of module %s first.\n"
        qualid.qual qualid.id qualid.qual;
    raise Toplevel
;;

let reserve_slot_for_variable qualid =
 let _ = get_slot_for_variable qualid in ()
;;

let get_slot_for_defined_variable qualid =
  if !toplevel then
    add_rollback (fun () -> remove_from_numtable !global_table qualid);
  enter_in_numtable !global_table qualid
;;

let reserve_slot_for_defined_variable qualid =
 let _ = get_slot_for_defined_variable qualid in ()
;;

let get_slot_for_literal cst =
  let c = (!global_table).num_cnt in
    (!global_table).num_cnt <- succ (!global_table).num_cnt;
    literal_table := (c, cst) :: !literal_table;
    c
;;

let number_of_globals () = (!global_table).num_cnt;;

(* The exception tags *)

let exn_tag_table = ref(new_numtable 1 : (qualified_ident * int) numtable)
and tag_exn_table = ref( [| |] : (qualified_ident * int) vect )
and unknown_exn_name = ({qual="?"; id="?"}, 0)
;;

let get_num_of_exn (name, stamp) =
  try
    hashtbl__find (!exn_tag_table).num_tbl (name, stamp)
  with Not_found ->
    let c = enter_in_numtable !exn_tag_table (name, stamp) in
    if c >= vect_length !tag_exn_table then begin
      let new_tag_exn_table =
        make_vect (2 * vect_length !tag_exn_table) unknown_exn_name in
      blit_vect !tag_exn_table 0
                new_tag_exn_table 0
                (vect_length !tag_exn_table);
      tag_exn_table := new_tag_exn_table
    end;
    (!tag_exn_table).(c) <- (name, stamp);
    c
;;

let reserve_num_of_exn (name, stamp) =
 let _ = get_num_of_exn (name, stamp) in ()
;;

let get_exn_of_num tag =
  if tag >= vect_length !tag_exn_table
  then unknown_exn_name
  else (!tag_exn_table).(tag)
;;

let get_num_of_tag = function
    ConstrRegular(n,_) -> n
  | ConstrExtensible(id, stamp) -> get_num_of_exn(id, stamp)
;;

(* The C primitives *)

let custom_runtime = ref false;;

let c_prim_table = ref (new_numtable 1 : string numtable);;

let set_c_primitives prim_vect =
  c_prim_table := new_numtable 31;
  do_vect (reserve_in_numtable !c_prim_table) prim_vect
;;

let get_num_of_prim name =
  try
    find_in_numtable !c_prim_table name
  with Not_found ->
    if !custom_runtime then
      enter_in_numtable !c_prim_table name
    else begin
      interntl__eprintf "The C primitive \"%s\" is not available.\n" name;
      raise Toplevel
    end
;;

let output_primitives oc =
  let prim = make_vect (!c_prim_table).num_cnt "" in
  hashtbl__do_table
    (fun name number -> prim.(number) <- name)
    (!c_prim_table).num_tbl;
  for i = 0 to vect_length prim - 1 do
    fprintf oc "extern long %s();\n" prim.(i)
  done;
  fprintf oc "typedef long (*primitive)();\n";
  fprintf oc "primitive cprim[] = {\n";
  for i = 0 to vect_length prim - 1 do
    fprintf oc "  %s,\n" prim.(i)
  done;
  fprintf oc "  0 };\n";
  fprintf oc "char * names_of_cprim[] = {\n";
  for i = 0 to vect_length prim - 1 do
    fprintf oc "  \"%s\",\n" prim.(i)
  done;
  fprintf oc "  (char *) 0 };\n";
  ()
;;

(* Initialization *)

let reset_linker_tables () =
  global_table := new_numtable 263;
  literal_table := [];
  do_list reserve_slot_for_defined_variable predef_variables;
  exn_tag_table := new_numtable 31;
  tag_exn_table := make_vect 50 unknown_exn_name;
  do_list reserve_num_of_exn predef_exn;
  set_c_primitives prim_c__primitives_table
;;


(* To write and read linker tables to a file *)

let save_linker_tables outstream =
  output_compact_value outstream !global_table;
  output_compact_value outstream !exn_tag_table;
  output_compact_value outstream !tag_exn_table

and load_linker_tables instream =
  global_table := input_value instream;
  exn_tag_table := input_value instream;
  tag_exn_table := input_value instream;
  ()
;;

