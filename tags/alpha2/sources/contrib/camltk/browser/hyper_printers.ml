#open "misc";;
#open "const";;
#open "globals";;
#open "modules";;
#open "builtins";;
#open "types";;

#open "tk";;



let buffer = ref ""
;;
let curpos = ref 0
;;
let tags = ref ([] : (int * int) list)
;;

let reset_state () =
  buffer := ""; curpos := 0; tags := []
;;


(* Basic printing function *)
let print_string s =
   buffer := !buffer ^ s;
   curpos := !curpos + (string_length s)
;;

let print_type_constructor {qualid = {qual=m; id = s}} = 
  let rec can_omit_qualifier = function
      [] -> false
    | md1::mdl ->
        try
          hashtbl__find (types_of_module md1) s;
          m = md1.mod_name
        with Not_found ->
          can_omit_qualifier mdl in
  let beg = !curpos in
    if not (can_omit_qualifier !used_modules) then
      (print_string m; print_string "__");
    print_string s;
    tags := (beg, !curpos)::!tags
;;

(********* Printing types **********)
(* Based on pr_type *)
(* identical to pr_type__print_typ *)
let rec int_to_alpha i =
  if i < 26
  then make_string 1 (char_of_int (i+96))
  else (int_to_alpha (i/26) ^ make_string 1 (char_of_int ((i mod 26)+97)))
;;

let reset_type_var_name, name_of_type_var =
  let vars = ref []
  and var_counter = ref 0 in
    (fun () -> vars := []; var_counter := 0; ()),
    (fun var ->
       try
         assq var !vars
       with Not_found ->
         incr var_counter;
         let var_name = int_to_alpha !var_counter in
           vars := (var, var_name) :: !vars; var_name)
;;

let rec print_typ priority ty =
  let ty = type_repr ty in
  match ty.typ_desc with
    Tvar _ ->
      print_string "'"; print_string (name_of_type_var ty)
  | Tarrow(ty1, ty2) ->
      if priority >= 1 then print_string "(";
      print_typ 1 ty1;
      print_string " -> ";
      print_typ 0 ty2;
      if priority >= 1 then print_string ")"
  | Tproduct(ty_list) ->
      if priority >= 2 then print_string "(";
      print_typ_list 2 " * " ty_list;
      if priority >= 2 then print_string ")"
  | Tconstr(cstr, args) ->
      begin match args with
         []    -> ()
       | [ty1] ->
           print_typ 2 ty1; print_string " "
       | tyl ->
           print_string "("; print_typ_list 0 ", " tyl; print_string ") "
       end;
       print_type_constructor cstr

and print_typ_list priority sep = function
    [] ->
      fatal_error "print_typ_list"
  | [ty] ->
      print_typ priority ty
  | ty::rest ->
      print_typ priority ty;
      print_string sep;
      print_typ_list priority sep rest
;;

(**************************************************)
(* Type definitions *)
(**************************************************)

(* Only for abstract types *)
let typ_of_tcg tyc =
  let ar = tyc.info.ty_arity in
  let vars = new_type_var_list ar in
    {typ_desc = Tconstr(tyc.info.ty_constr, vars);
     typ_level = generic}
;;
  


let print_type_desc 
    ({qualid = {qual=modname; id=_};
      info   = {ty_desc = ty_c; _ }} as tcg) =

  reset_type_var_name ();

  (* Find out a polymorphic form of the constructor *)
  let typ_of_constr = match ty_c with
      Abstract_type    -> typ_of_tcg tcg
   |  Variant_type (x::l) -> x.info.cs_res
   |  Variant_type ([]) -> type_exn  (* puke *)
   |  Record_type (x::l)  -> x.info.lbl_res
   |  Abbrev_type (l,body) -> {typ_desc = Tconstr(tcg.info.ty_constr,l);
       	       	       	       typ_level = generic} in
      
    print_string "type ";
    print_typ 0 typ_of_constr;
    tags := tl !tags; (* dump the constructor anchor *)
    print_string " =";      
    begin match ty_c with
      Abstract_type -> print_string " abstract\n"
    | Variant_type ([]) -> print_string " extensible\n"
    | Variant_type (x::l) -> 
       	 let print_constructor cstr =
	   begin match  cstr.info.cs_mut with
	    Mutable -> print_string "mutable "
	  | _ -> ()
           end;
       	   print_string cstr.qualid.id;
	   begin match cstr.info.cs_kind with
	    Constr_constant -> print_string "\n" 
	  | _ -> begin
		  print_string " of ";
		  let arg, res = type_pair_instance (cstr.info.cs_arg,
      	       	       	       	       	       	   cstr.info.cs_res) in
		   unify (res, typ_of_constr);
		  print_typ 0 arg;
		  print_string "\n" 
      	         end 
           end in
	 print_string "\n\t\t";
	 print_constructor x;
	 do_list (fun c -> 
      	       	   print_string "\t|\t";
      	       	   print_constructor c) l
     |  Record_type (x::l)  -> 
       	 let print_label lbl =
	   begin match  lbl.info.lbl_mut with
	    Mutable -> print_string "mutable "
	  | _ -> ()
           end;
	   print_string lbl.qualid.id;
	   print_string " : ";
	   print_typ 0 lbl.info.lbl_arg in
	 print_string " {\n\t";
	 print_label x;
	 do_list (fun lbl ->
	           print_string ";\n\t";
		   print_label lbl) l;
	 print_string "\n}\n"
     |  Abbrev_type (_,body) -> 
       	 print_string "= ";
	 print_typ 0 body;
	 print_string "\n"
    end;
    print_string ";;\n"
;;



(* Values *)
let print_value_desc
      {qualid = {qual=modname; id=sym};
       info   = {val_typ = ty; val_prim=prim}}  = 

  reset_type_var_name ();
  begin match prim with 
     ValueNotPrim -> print_string ("value "^sym^" : ")
   | ValuePrim(_,_) -> print_string ("primitive "^sym^" : ")
  end;
  print_typ 0 ty;
  print_string "\n;;\n"
;;



(* Exceptions *)
let print_exc_desc 
       {qualid = {qual=modname; id=sym};
        info   = {cs_arg = ty; cs_mut=mut}}  =
  reset_type_var_name ();
  begin match mut with
    Mutable -> print_string ("exception "^sym^" of mutable ")
  | Notmutable -> print_string ("exception "^sym^" of ")
  end;
  print_typ 0 ty;
  print_string "\n;;\n"
;;

(* Constructors *)
(* print also result to nagivate to other constructors *)
let print_constr_desc 
       {qualid = {qual=modname; id=sym};
        info   = {cs_arg = ty; cs_res = ty'; cs_mut=mut}}  =
  reset_type_var_name ();
  begin match mut with
    Mutable -> print_string ("constructor "^sym^" of mutable ")
  | Notmutable -> print_string ("constructor "^sym^" of ")
  end;
  print_typ 0 ty;
  print_string " : ";
  print_typ 0 ty';
  print_string "\n;;\n"
;;


(* Labels *)
let print_label_desc 
       {qualid = {qual=modname; id=sym};
        info   = {lbl_arg = ty; lbl_mut=mut}}  =
  reset_type_var_name ();
  begin match mut with
    Mutable -> print_string ("mutable "^sym^" : ")
  | Notmutable -> print_string (sym^" : ")
  end;
  print_typ 0 ty;
  print_string "\n;;\n"
;;



let print_module m =
  hashtbl__do_table (fun _ td -> print_type_desc td) (types_of_module m);
  hashtbl__do_table (fun _ vd -> print_value_desc vd) (values_of_module m);
  hashtbl__do_table (fun _ cd -> 
      	       	      if type_exn = cd.info.cs_res
      	       	      then print_exc_desc cd ) (constrs_of_module m)
;;

(**************************************************)
(* The hypertext widget *)
(**************************************************)

let abs_index n =
  TextIndex (TI_LineChar(0,0), [CharOffset n])
;;

(* Back hack due to non-polymorphic letrec... *)
(* I cannot have a visual_meta abstraction depending on
   one of its instance and still keep visual_meta polymorphic 
*)

let hyper_action = ref print_string;;

let anchor_attrib = ref [Foreground Blue]
;;

let hypertext top f arg =
  reset_state ();
  f arg;
  let t = text__create top []  in
  text__insert t (TextIndex (TI_End, [])) !buffer;
  do_list (function (b,e ) ->
      	    text__tag_add t "hyper" (abs_index b) (abs_index e))
          !tags;
  text__tag_configure t "hyper" !anchor_attrib;
  (* Disable editing *)
  text__configure t [State Disabled];

  (* Assume mark "current" is in anchor *)
  let get_current_anchor () =
    let b,e = text__tag_nextrange t "hyper" 
      	       	    (TextIndex (TI_Mark "current", [WordStart]))
		    (TextIndex (TI_End, [])) in
    text__get t (TextIndex(b,[]))  (TextIndex(e,[])) in
    
  text_tag_bind t "hyper" [[Double], WhatButton 1] 
      (BindSet ([], fun _ -> !hyper_action (get_current_anchor())));
  (* Make reasonable size *)
  let i = text__index t (TextIndex(TI_End,[])) in
  let lines = 
     match i with
      TI_LineChar(l,c) -> l
    | _ -> failwith "subliminal" (* not suppose to happen *) in
  text__configure t [TextHeight (if lines < 15 then lines else 15)];

  t
;;

