#open "misc";;
#open "const";;
#open "globals";;
#open "modules";;

#open "tk";;
#open "support";;

#open "hyper_printers";;
#open "source";;
#open "tags";;

(* Namer for toplevel widgets *)
let new_visual_top =
  let cnter = ref 0 in
  function () ->
   incr cnter; "global" ^ string_of_int !cnter
;;

(* Abstraction for displaying an object of type 'a associated to a symbol  *)
type 'a visual =
    { titler : 'a -> string;		(* title from object *)
      finder : string -> 'a;		(* finding an object *)
      hyperprinter : 'a -> hypertext;   (* printing the object *)
      hypernav : 'a -> hypernav;        (* navigation *)
      editor : 'a -> unit		(* further external edition *)
    }
;;

(* Display on object associated to sym according to abstraction visual *)    

let rec visual_meta visual silent sym =
  try
    (* Find the object *)
    let object = visual.finder sym in

    let t = toplevelw__create 
      	       (new_toplevel_widget (new_visual_top ())) [] in

    let titlestr = visual.titler object in
    wm__title_set t titlestr;
    (* busy *)
    let title =
         label__create t [Text titlestr; Relief Raised; 
      	       	       	  Cursor (XCursor "watch")] in
     pack [title][];
     (* this should display the title, but it doesn't *)
    update_idletasks();

    let f = frame__create t [] in
    let tx = 
       hypertext f (visual.hypernav object) (visual.hyperprinter object) in
    let sb = scrollbar__create f [] in
      util__scroll_text_link sb tx;
      pack [tx] [Side Side_Left; Fill Fill_Both; Expand true];
      pack [sb] [Side Side_Left; Fill Fill_Y];
      util__navigation_keys tx sb;

    let f2 = frame__create t [] in
    let edit =
      button__create f2 [Text "Edit"; Relief Raised;
      	       	       	 Command (fun _ -> visual.editor object)]
    and q = 
      button__create f2 [Text "Ok"; Relief Raised; 
      	       	        Command (fun _ -> destroy t)] in

    (* An easy way to quit *)
    bind tx [[Any],XKey "Escape"] 
      	     (BindSet([], (fun _ -> button__invoke q)));
    pack__forget [title];
    pack [f] [Fill Fill_Both; Expand true];
     pack [edit] [Side Side_Left];
     pack [q] [Side Side_Right; Fill Fill_X; Expand true];
    pack [f2] [Side Side_Bottom; Fill Fill_X];
    util__resizeable t
   with   
    Toplevel -> begin
	 let _ = 
           dialog (new_toplevel_widget "error")
	     "Caml Browser Error"
	     ( "Cannot open module :" ^ sym)
	     (Predefined "error")
	     0
	     ["Ok"] in
	 ()
	end
   | Desc_not_found -> 
      	if not silent then begin
	  let _ = 
           dialog (new_toplevel_widget "error")
	      "Caml Browser Error"
	      ( sym ^ " is undefined")
	      (Predefined "error")
	      0
	      ["Ok"]; in
         ()
         end
   | Cannot_find_file filename ->
      begin 
       let _ = 
         dialog (support__new_toplevel_widget "error")
      	      "Caml Browser Error"
	      ("Cannot open " ^ filename )
	      (Predefined "error") 0 ["Ok"]; in
       ()
      end
;;

(* Parsing a qualified ident by hand ... *)
let cindex p s start = find start
  where rec find i =
    if i >= string_length s 
    then raise Not_found
    else if p (nth_char s i) then i 
    else find (i+1) 
;;

let is_underscore c = c == `_`
;;

let str_to_qual s =
  let l = string_length s in
  let rec advance cur =
    let c = cindex is_underscore s cur in
    let c1 = succ c in
      if c1 >= l then raise Not_found
      else if is_underscore (nth_char s c1) then
      	    GRmodname {qual = sub_string s 0 c;
      	       	       id =   sub_string s (succ c1) (l - succ c1)}
	   else advance (succ c1) in
  try advance 0
  with Not_found -> GRname s
;;

(* Associate a title to a global *)
let global_titler what g =
  what ^ " " ^ g.qualid.qual ^ "__" ^ g.qualid.id
;;


let source_editor m = 
  function _ ->
    try
    let m = misc__find_in_path m in
    let _ = sys__system_command ("$EDITOR "^m^"&") in ()
    with
      _ -> ()
;;

(* This should be configurable *)
let default_anchor_attribs = ref [];;

(* in monochrome: all true anchors are underlined *)
let use_monochrome_attribs () =
  default_anchor_attribs :=
   ["hypertype", [Underline true];
    "hypersource", [Underline true];
    "keyword", [];
    "comment", [];
    "modules", [Underline true]
   ]
(* in color: true anchors are in blue shades *)
and use_color_attribs () =
  default_anchor_attribs :=
   ["hypertype", [Foreground Blue];
    "hypersource", [Foreground (NamedColor "purple")];
    "keyword", [Foreground (NamedColor "SeaGreen")];
    "comment", [Foreground (NamedColor "brown")];
    "modules", [Foreground Blue]
   ]
;;

(* The navigators *)
(* Monomorphic let rec hits again : one global_navigators should be enough *)
(*  anchor "hypertype"  : we know it is a type so search only in types *)
(*  anchor "hypersource": we don't know what it is, and we want to se  *)
(*                       its source, if possible. Use Emacs TAGS       *)

let rec type_navigators desc =
  { anchor_attribs = !default_anchor_attribs;
    navigators =
      	["hypertype", visual_type false; 
      	 "hypersource", visual_source desc.qualid.qual]
  }
and constr_navigators desc =
  { anchor_attribs = !default_anchor_attribs;
    navigators =
      	["hypertype", visual_type false; 
      	 "hypersource", visual_source desc.qualid.qual]
  }
and label_navigators desc =
  { anchor_attribs = !default_anchor_attribs;
    navigators =
      	["hypertype", visual_type false; 
      	 "hypersource", visual_source desc.qualid.qual]
  }
and value_navigators desc =
  { anchor_attribs = !default_anchor_attribs;
    navigators =
      	["hypertype", visual_type false; 
      	 "hypersource", visual_source desc.qualid.qual]
  }

(* Visualisation of type definitions *)
and visual_type f d =
    visual_meta {titler = global_titler "Type";
      	       	 finder = (function s -> find_type_desc (str_to_qual s));
		 hyperprinter = hyper_print print_type_desc;
      	       	 hypernav = type_navigators;
		 editor = (function _ -> ())
                } f d

(* Visualisation of value constructors *)
and visual_constr f d =
    visual_meta {titler = global_titler "Constructor";
      	       	 finder = (function s -> find_constr_desc (str_to_qual s));
		 hyperprinter = hyper_print print_constr_desc;
                 hypernav = constr_navigators;
		 editor = (function _ -> ())
                } f d
(* Visualisation of values *)
and visual_value f d =
    visual_meta {titler = global_titler "Value";
      	       	 finder = (function s -> find_value_desc (str_to_qual s));
		 hyperprinter = hyper_print print_value_desc;
      	       	 hypernav = value_navigators;
		 editor = (function _ -> ())
                } f d
(* Visualisation of labels *)
and visual_label f d =
    visual_meta {titler = global_titler "Label";
      	       	 finder = (function s -> find_label_desc (str_to_qual s));
		 hyperprinter = hyper_print print_label_desc;
      	       	 hypernav = label_navigators;
		 editor = (function _ -> ())
                } f d
(* Combined search, does not expect to find objects *)
and visual_search_any s =
  visual_type true s;
  visual_constr true s;
  visual_value true s;
  visual_label true s

(* Navigation when we browse source files (.ml and .mli) *)
and source_navigator _ = { 
    anchor_attribs = !default_anchor_attribs;
    navigators = [ "", visual_search_any;
      	       	   "modules", visual_module true]}

(* Visualize source for s in module m *)
and visual_source_ml m s =
 try 
  let h1 = hashtbl__find dotml_emacs_tags m in
  let linenum = hashtbl__find h1 s in
    visual_meta {titler = (function _ -> m ^ ".ml") ;
                 finder = (function _ -> load_file (m^".ml"));
                 hyperprinter = 
      	       	    (function src -> 
      	       	       	{text = src;
		         start_line = linenum;
			 anchor_indexes= compute_source_tags src});
		 hypernav = source_navigator;
		 editor = source_editor (m ^".ml")}
                true 
                s
 with
  _ -> ()

(* Visualize interface for s in module m *)
and visual_source_mli m s =
 try 
  let h1 = hashtbl__find dotmli_emacs_tags m in
  let linenum = hashtbl__find h1 s in
    visual_meta {titler = (function _ -> m ^ ".mli") ;
                 finder = (function _ -> load_file (m^".mli"));
                 hyperprinter = 
      	       	    (function s -> {text = s;
		                    start_line = linenum;
				    anchor_indexes= compute_source_tags s});
		 hypernav = source_navigator;
		 editor = source_editor (m ^ ".mli")}
                true 
                s
 with
  _ -> ()

(* Visual whatever source is available for s in m *)
and visual_source m s =
  visual_source_ml m s;
  visual_source_mli m s

(* Display a source file (.ml or .mli) *)
and display_file f =
    visual_meta {titler = (function _ -> f) ;
                 finder = (function _ -> load_file f);
                 hyperprinter = 
      	       	    (function s -> {text = s;
		                    start_line = 0;
				    anchor_indexes= compute_source_tags s});
		 hypernav = source_navigator;
		 editor = source_editor f}
                true 
                f
(* Visualize a full module from its compiled interface *)
and module_navigators m =
  { anchor_attribs = !default_anchor_attribs;
    navigators =
      	["hypertype", visual_type false; 
      	 "hypersource", visual_source m.mod_name]
  }


and visual_module f d =
    visual_meta {titler = (function m -> "Module " ^ m.mod_name);
      	         finder = find_module;
		 hyperprinter = hyper_print print_module;
      	       	 hypernav = module_navigators;
		 editor = (function _ -> ());
                } f d
;;
