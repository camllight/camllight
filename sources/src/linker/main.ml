(* The Caml Light linker. Command-line parsing. *)

#open "config";;
#open "misc";;
#open "symtable";;
#open "link";;

let object_files = ref ([] : string list)
and exec_file = ref default_exec_name
and prim_file = ref ""
;;

let anonymous s =
  let name =
    if filename__check_suffix s ".ml" then
      filename__chop_suffix s ".ml" ^ ".zo"
    else
      s in
  object_files := name :: !object_files
;;
let set_stdlib p =
  path_library := p;
  load_path := [!path_library]
and add_include d =
  load_path := d :: !load_path
and set_debug () =
  write_symbols := true
and set_exec_file e =
  exec_file := e
and set_custom f =
  custom_runtime := true;
  prim_file := f
and show_version () =
  prerr_string version__banner; prerr_endline ""; exit 0
and process_include filename =
  do_list anonymous (readword__from_file filename)
and process_require filename =
  let rec require = function
    [] ->
      ()
  | "val"::qual::id::rest ->
      require_qualid qual id; require rest
  | "prim"::name::rest ->
      let n = get_num_of_prim name in require rest
  | _ ->
      fatal_error "bad require file"
  in require (readword__from_file filename)
;;

let main() =
try
  sys__catch_break true;
  load_path := [!path_library];
  reset_linker_tables();
  arg__parse ["-stdlib", arg__String set_stdlib;
              "-I", arg__String add_include;
              "-include", arg__String add_include;
              "-g", arg__Unit set_debug;
              "-debug", arg__Unit set_debug;
              "-o", arg__String set_exec_file;
              "-exec", arg__String set_exec_file;
              "-custom", arg__String set_custom;
              "-v", arg__Unit show_version;
              "-version", arg__Unit show_version;
              "-files", arg__String process_include;
              "-require", arg__String process_require;
              "-", arg__String anonymous]
             anonymous;
  link (rev !object_files) !exec_file;
  if !custom_runtime then begin
    let oc = open_out !prim_file in
    output_primitives oc;
    close_out oc
  end;
  exit 0

with Toplevel -> exit 2
   | sys__Break -> exit 3
   | Zinc s -> prerr_string "# Internal error: "; prerr_endline s; exit 4
;;

printexc__f main ()
;;
