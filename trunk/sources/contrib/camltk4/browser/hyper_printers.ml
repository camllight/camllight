#open "misc";;
#open "const";;
#open "globals";;
#open "modules";;
#open "builtins";;
#open "types";;

#open "tk";;


(******* Printing buffer *********)
let buffer = ref (create_string 1024)
and buffer_len = ref 1024
;;
let curpos = ref 0
;;
let tags = ref ([] : (int * int) list)	       (* .zi navigation *)
and source_tags = ref ([] : (int * int) list)  (* source navigation *)
;;

let reset_state () =
  curpos := 0; 
  tags := [];
  source_tags := []
;;


(* Basic printing function *)
let print_string s =
  let sl = string_length s in
  if sl + !curpos > !buffer_len then begin
      buffer:= !buffer ^ (create_string (1024 + sl));
      buffer_len := !buffer_len + 1024 + sl;
      end;
   blit_string s 0 !buffer !curpos sl;
   curpos := !curpos + sl
;;

let get_print_buffer () = 
  sub_string !buffer 0 !curpos
;;

let print_tag_string s =
  let beg = !curpos in
    print_string s;
    source_tags := (beg,!curpos)::!source_tags
;;


let print_type_constructor gl = 
  let beg = !curpos in
    if not (can_omit_qualifier types_of_module gl) then
      (print_string gl.qualid.qual; print_string "__");
    print_string gl.qualid.id;
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
   |  Record_type []  -> fatal_error "empty record type"
   |  Abbrev_type (l,body) -> {typ_desc = Tconstr(tcg.info.ty_constr,l);
       	       	       	       typ_level = generic} in
      
    print_string "type ";
    print_typ 0 typ_of_constr;
    tags := tl !tags; (* dump the constructor anchor *)
    print_string " =";      
    begin match ty_c with
      Abstract_type -> print_string " abstract\n"
    | Variant_type ([]) -> print_string " extensible\n"
    | Variant_type l -> 
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
       let sorted_l = 
      	 sort__sort (fun c c' -> le_string c.qualid.id c'.qualid.id) l in
       print_string "\n\t\t";
       print_constructor (hd sorted_l);
       do_list (fun c -> 
		 print_string "\t|\t";
		 print_constructor c) (tl sorted_l)
     |  Record_type l  -> 
       	 let print_label lbl =
	   begin match  lbl.info.lbl_mut with
	    Mutable -> print_string "mutable "
	  | _ -> ()
           end;
	   print_string lbl.qualid.id;
	   print_string " : ";
	   print_typ 0 lbl.info.lbl_arg in
        let sorted_l =
	  sort__sort (fun l l' -> le_string l.qualid.id l'.qualid.id) l in
	 print_string " {\n\t";
	 print_label (hd sorted_l);
	 do_list (fun lbl ->
	           print_string ";\n\t";
		   print_label lbl) (tl sorted_l);
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
     ValueNotPrim -> print_string "value ";
      	       	     print_tag_string sym;
      	       	     print_string " : "
   | ValuePrim(_,_) -> print_string "primitive ";
      	       	       print_tag_string sym;
      	       	       print_string " : "
  end;
  print_typ 0 ty;
  print_string "\n;;\n"
;;



(* Exceptions *)
let print_exc_desc 
       {qualid = {qual=modname; id=sym};
        info   = {cs_arg = ty; cs_mut=mut}}  =
  reset_type_var_name ();
  print_string "exception ";
  print_tag_string sym;
  begin match mut with
    Mutable -> print_string " of mutable "
  | Notmutable -> print_string " of "
  end;
  print_typ 0 ty;
  print_string "\n;;\n"
;;

(* Constructors *)
(* print also result to navigate to other constructors *)
let print_constr_desc 
       {qualid = {qual=modname; id=sym};
        info   = {cs_arg = ty; cs_res = ty'; cs_mut=mut}}  =
  reset_type_var_name ();
  print_string "constructor ";
  print_tag_string sym;
  begin match mut with
    Mutable -> print_string " of mutable "
  | Notmutable -> print_string " of "
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
    Mutable -> print_string "mutable "
  | Notmutable -> ()
  end;
  print_tag_string sym;
  print_string " : ";
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

(* Data *)
type hypertext = {
  text : string;			(* text to be displayed *)
  start_line : int;
  anchor_indexes :			(* anchor lists *)
      (string * (int * int) list) list
}
;;

(* wrapper for our previous printers *)
let hyper_print f x =
  reset_state();
  f x;
  let res = {
    text = get_print_buffer();
    start_line = 0;
    anchor_indexes = ["hypertype", !tags; "hypersource", !source_tags]
    } in
    (* take it easy on GC *)
  reset_state ();
  res
;;

(* convert an integer to an absolute index *)
let abs_index n =
  TextIndex (LineChar(0,0), [CharOffset n])
;;


(* Navigation properties *)
(* we allow several tags with different display and actions *)
type hypernav = {
  anchor_attribs : (string * options list) list;    (* chrome *)
  navigators: (string * (string -> unit)) list     (* hyper actions *)
  }
;;

(* Build an hypertext widget, given a 
   top (Widget frame),
   hn  (hypernav record),
   ht  (hypertext record)
*)
let hypertext top hn ht =
  let t = text__create top []  in
  (* Insert the text *)
  text__insert t (TextIndex (End, [])) ht.text [];
  (* Define the anchors *)
  do_list (function (tagname, l) ->
      	     do_list (function (b,e) -> 
      	       	       	text__tag_add t tagname (abs_index b) (abs_index e))
                       l)
          ht.anchor_indexes;
  (* Configure the anchors *)
  do_list (function (tagname, options) ->
      	       text__tag_configure t tagname options)
           hn.anchor_attribs;

  (* Disable editing *)
  text__configure t [State Disabled];
  text__yview_line t ht.start_line;
  (* Assume mark "current" is in anchor
   * Note: there is no way to tell Tk what the syntax table is. Operators
   * like == and <= will confuse our code. It seems also there is no way
   * to get the tag region in the context of a tag callback.
   *)
  let get_current_anchor tagname =
    let b,e = match tagname with
       "" -> text__index t (TextIndex (Mark "current", [WordStart])),
             text__index t (TextIndex (Mark "current", [WordEnd]))
    | tagname -> 
      	try
          text__tag_nextrange t tagname
      	       	    (TextIndex (Mark "current", [WordStart]))
		    (TextIndex (End, [])) 
        with
      	  _ -> 
           text__index t (TextIndex (Mark "current", [WordStart])),
           text__index t (TextIndex (Mark "current", [WordEnd]))  in
    text__get t (TextIndex(b,[]))  (TextIndex(e,[])) in
  (* Setup the callbacks *)
  do_list
      (function 
         ("", action) -> 
           bind t [[Double], ButtonPressDetail 1] 
	     (BindExtend ([], fun _ -> action (get_current_anchor "")))
       | (tagname, action) ->
      	   text__tag_bind t tagname [[Double], ButtonPressDetail 1]
	     (BindSet ([], function _ -> action (get_current_anchor tagname))))
      hn.navigators;
    
  (* Make reasonable size *)
  let i = text__index t (TextIndex(End,[])) in
  let lines = 
     match i with
      LineChar(l,c) -> l
    | _ -> failwith "subliminal" (* not suppose to happen *) in
  text__configure t [TextHeight (if lines < 15 then lines else 15)];

  t
;;
