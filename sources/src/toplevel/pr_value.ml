(* To print values *)

#open "obj";;
#open "misc";;
#open "const";;
#open "globals";;
#open "builtins";;
#open "modules";;
#open "types";;
#open "pr_type";;
#open "symtable";;

let rec find_constr tag = function
    [] ->
      fatal_error "find_constr: unknown constructor for this type"
  | constr::rest ->
      match constr.info.cs_tag with
        ConstrRegular(t, _) ->
          if t == tag then constr else find_constr tag rest
      | ConstrExtensible _ ->
          fatal_error "find_constr: extensible"
;;

let find_exception tag =
  let (qualid, stamp) = get_exn_of_num tag in
  let rec select_exn = function
    [] ->
      raise Not_found
  | ({info = {cs_tag = ConstrExtensible(_,st)}} as desc) :: rest ->
      if st == stamp then desc else select_exn rest in
  select_exn(hashtbl__find_all (find_module qualid.qual).mod_constrs qualid.id)
;;

let print_counter = ref 0
;;

exception Ellipsis
;;

let printers = ref [
  constr_type_int,
    (fun x -> print_int (magic_obj x : int));
  constr_type_float,
    (fun x -> print_float(magic_obj x : float));
  constr_type_char,
    (fun x -> print_string "`";
              print_string (char_for_read (magic_obj x : char));
              print_string "`");
  constr_type_string,
   (fun x -> print_string "\"";
             print_string (string_for_read (magic_obj x : string));
             print_string "\"")
];;

let rec print_val prio obj ty =
  decr print_counter;
  if !print_counter < 0 then raise Ellipsis;
  match (type_repr ty).typ_desc with
    Tvar _ ->
      print_string "<poly>"
  | Tarrow(ty1, ty2) ->
      print_string "<fun>"
  | Tproduct(ty_list) ->
      if prio > 0 then print_string "(";
      print_val_list 1 obj ty_list;
      if prio > 0 then print_string ")"
  | Tconstr(cstr, ty_list) ->
      if same_type_constr cstr constr_type_list then begin
        let ty_arg = 
          match ty_list with [ty] -> ty | _ -> fatal_error "print_val (list)"
        in
          print_string "[";
          let rec print_conses cons =
            if obj_tag cons == 0 then
              print_string "]"
            else begin
              print_val 0 (obj_field cons 0) ty_arg;
              if obj_tag (obj_field cons 1) == 0 then
                print_string "]"
              else begin
                print_string "; "; print_conses (obj_field cons 1)
              end
            end
          in print_conses obj
      end else if same_type_constr cstr constr_type_vect then begin
        let ty_arg = 
          match ty_list with [ty] -> ty | _ -> fatal_error "print_val (vect)"
        in
          print_string "[|";
          for i = 0 to obj_size obj - 1 do
            if i > 0 then print_string "; ";
            print_val 0 (obj_field obj i) ty_arg
          done;
          print_string "|]"
      end else
        try
          let rec find_printer = function
            [] ->
              raise Not_found
          | (cstr', f) :: rest ->
              if same_type_constr cstr cstr'
              then f obj
              else find_printer rest
          in
            find_printer !printers
        with Not_found ->
          match cstr.info.ty_abbr with
            Tabbrev(params, body) ->
              print_val prio obj (expand_abbrev params body ty_list)
          | _ ->
              print_concrete_type prio obj cstr ty ty_list

and print_concrete_type prio obj cstr ty ty_list =
  let typ_descr =
    type_descr_of_type_constr cstr in
  match typ_descr.info.ty_desc with
    Abstract_type ->
      print_string "<abstract>"
  | Variant_type constr_list ->
      let tag = obj_tag obj in
      begin try
        let constr = 
          if same_type_constr cstr constr_type_exn
          then find_exception tag
          else find_constr tag constr_list in
        let (ty_res, ty_arg) =
          type_pair_instance (constr.info.cs_res, constr.info.cs_arg) in
        filter (ty_res, ty);
        match constr.info.cs_kind with
          Constr_constant ->
            print_constr constr
        | Constr_regular ->
            if prio > 1 then print_string "(";
            print_constr constr;
            print_string " ";
            print_val 2 (obj_field obj 0) ty_arg;
            if prio > 1 then print_string ")"
        | Constr_superfluous n ->
            if prio > 1 then print_string "(";
            print_constr constr;
            print_string " (";
            print_val_list 1 obj (filter_product n ty_arg);
            print_string ")";
            if prio > 1 then print_string ")"
      with
        Not_found ->
          print_string "<local exception>"
      | Unify ->
          fatal_error "print_val: types should match"
      end
  | Record_type label_list ->
      print_string "{";
      let print_field lbl =
        print_label lbl;
        print_string "=";
        let (ty_res, ty_arg) =
          type_pair_instance (lbl.info.lbl_res, lbl.info.lbl_arg) in
        begin try
          filter (ty_res, ty)
        with Unify ->
          fatal_error "print_val: types should match"
        end;
        print_val 0 (obj_field obj lbl.info.lbl_pos) ty_arg in
      let rec print_fields = function
        [] -> ()
      | [lbl] -> print_field lbl
      | lbl::rest ->
          print_field lbl; print_string "; "; print_fields rest
      in
        print_fields label_list; print_string "}"
  | Abbrev_type(_,_) ->
      fatal_error "print_val: abbrev type"

and print_val_list prio obj ty_list =
  let rec print_list i = function
    [] ->
      fatal_error "print_val_list"
  | [ty] ->
       print_val prio (obj_field obj i) ty
  | ty::ty_list ->
       print_val prio (obj_field obj i) ty;
       print_string ", ";
       print_list (succ i) ty_list
  in
    print_list 0 ty_list
;;

let print_value obj ty =
  print_counter := 100;
  try
    print_val 0 obj ty
  with Ellipsis ->
    print_string "..."
;;


