(* To print values *)

#open "obj";;
#open "misc";;
#open "const";;
#open "globals";;
#open "builtins";;
#open "modules";;
#open "types";;
#open "format";;
#open "fmt_type";;
#open "symtable";;

exception Constr_not_found;;

let rec find_constr tag = function
    [] ->
      raise Constr_not_found
  | constr::rest ->
      match constr.info.cs_tag with
        ConstrRegular(t, _) ->
          if t == tag then constr else find_constr tag rest
      | ConstrExtensible _ ->
          fatal_error "find_constr: extensible"
;;

exception Exception_not_found;;

let find_exception tag =
  let (qualid, stamp) = get_exn_of_num tag in
  let rec select_exn = function
    [] ->
      raise Exception_not_found
  | constr :: rest ->
      match constr.info.cs_tag with
        ConstrExtensible(_,st) ->
          if st == stamp then constr else select_exn rest
      | ConstrRegular(_,_) ->
          fatal_error "find_exception: regular" in
  select_exn(hashtbl__find_all (find_module qualid.qual).mod_constrs qualid.id)
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

let printer_depth = ref 100
;;

exception Ellipsis
;;

let cautious f arg = try f arg with Ellipsis -> print_string "...";;

let rec print_val prio depth obj ty =
  if depth < 0 then raise Ellipsis;
  match (type_repr ty).typ_desc with
    Tvar _ ->
      print_string "<poly>"
  | Tarrow(ty1, ty2) ->
      print_string "<fun>"
  | Tproduct(ty_list) ->
      if prio > 0 then begin open_hovbox 1; print_string "(" end
       else open_hovbox 0;
      print_val_list 1 depth obj ty_list;
      if prio > 0 then print_string ")";
      close_box()
  | Tconstr(cstr, ty_list) ->
      if same_type_constr cstr constr_type_list then begin
        let ty_arg = 
          match ty_list with [ty] -> ty | _ -> fatal_error "print_val (list)"
        in
           let rec print_conses depth cons =
            if obj_tag cons != 0 then begin
              print_val 0 depth (obj_field cons 0) ty_arg;
              if obj_tag (obj_field cons 1) != 0 then begin
                print_string ";"; print_space();
                print_conses (depth - 1) (obj_field cons 1)
              end
            end
          in
           begin
            open_hovbox 1;
            print_string "[";
            cautious (print_conses (depth - 1)) obj;
            print_string "]";
            close_box()
           end

      end else if same_type_constr cstr constr_type_vect then begin
        let ty_arg = 
          match ty_list with [ty] -> ty | _ -> fatal_error "print_val (vect)"
        in
          let print_items depth obj =
             for i = 0 to obj_size obj - 1 do
              if i > 0 then print_string ";"; print_space();
              print_val depth 0 (obj_field obj i) ty_arg
             done
         in
          open_hovbox 2;
          print_string "[|";
          cautious (print_items (depth - 1)) obj;
          print_string "|]";
          close_box()
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
               begin match cstr.info.ty_abbr with
                 Tabbrev(params, body) ->
                   cautious (print_val prio depth obj)
                            (expand_abbrev params body ty_list)
               | _ ->
                   print_concrete_type prio depth obj cstr ty ty_list end

and print_concrete_type prio depth obj cstr ty ty_list =
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
            output_constr constr
        | Constr_regular ->
            if prio > 1 then begin open_hovbox 2; print_string "(" end
             else open_hovbox 1;
            output_constr constr;
            print_space();
            cautious (print_val 2 (depth - 1) (obj_field obj 0)) ty_arg;
            if prio > 1 then print_string ")";
            close_box()
        | Constr_superfluous n ->
            if prio > 1 then begin open_hovbox 2; print_string "(" end
            else open_hovbox 1;
            output_constr constr;
            print_space();
            open_hovbox 1;
            print_string "(";
            print_val_list 1 (depth - 1) obj (filter_product n ty_arg);
            print_string ")";
            close_box();
            if prio > 1 then print_string ")";
            close_box()
      with
        Constr_not_found ->
          print_string "<unknown constructor>"
      | Exception_not_found ->
          print_string "<local exception>"
      | Unify ->
          fatal_error "print_val: types should match"
      end
  | Record_type label_list ->
      let print_field depth lbl =
        open_hovbox 1; 
        output_label lbl;
        print_string "="; print_cut();
        let (ty_res, ty_arg) =
          type_pair_instance (lbl.info.lbl_res, lbl.info.lbl_arg) in
        begin try
          filter (ty_res, ty)
        with Unify ->
          fatal_error "print_val: types should match"
        end;
        print_val 0 depth (obj_field obj lbl.info.lbl_pos) ty_arg;
        close_box() in
      let rec print_fields depth = function
        [] -> ()
      | [lbl] -> print_field depth lbl
      | lbl::rest ->
          print_field depth lbl; print_string ";"; print_space();
          print_fields depth rest
      in
      open_hovbox 1;
      print_string "{";
      cautious (print_fields (depth - 1)) label_list;
      print_string "}";
      close_box()
  | Abbrev_type(_,_) ->
      fatal_error "print_val: abbrev type"

and print_val_list prio depth obj ty_list =
  let rec print_list depth i = function
    [] ->
      fatal_error "print_val_list"
  | [ty] ->
       print_val prio depth (obj_field obj i) ty
  | ty::ty_list ->
       print_val prio depth (obj_field obj i) ty;
       print_string ","; print_space();
       print_list depth (succ i) ty_list
  in cautious (print_list (depth - 1) 0) ty_list
;;

let print_value obj ty = print_val 0 !printer_depth obj ty
;;


