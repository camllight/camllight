#open "misc";;
#open "const";;
#open "globals";;
#open "modules";;
#open "hyper_printers";;
#open "tk";;

let new_visual_top =
  let cnter = ref 0 in
  function () ->
   incr cnter; "global" ^ string_of_int !cnter
;;


type 'a visual =
    { namer : 'a -> string;
      finder : string -> 'a;
      hyperprinter : 'a -> unit
    }
;;
    
let rec visual_meta visual silent sym =
  try
    let desc = visual.finder sym in
    let t = toplevelw__create 
      	       (support__new_toplevel_widget (new_visual_top ())) [] in
    let title =
      label__create t [Text (visual.namer desc); Relief Raised] in
    pack [title] [Fill Fill_X];
    label__configure title [Cursor (XCursor "watch")];
    update_idletasks();
    let f = frame__create t [] in
    let tx = hypertext f visual.hyperprinter desc in
    let sb = scrollbar__create f [] in
      util__scroll_text_link sb tx;
      pack [tx] [Side Side_Left; Fill Fill_Both; Expand true];
      pack [sb] [Side Side_Left; Fill Fill_Y];
      (* This does not work at the moment *)
      util__navigation_keys tx sb;

    let q = 
      button__create t [Text "Ok"; Relief Raised; 
      	       	        Command (fun _ -> destroy t)] in
    bind tx [[Any],XKey "Escape"] 
      	     (BindSet([], (fun _ -> button__invoke q)));
    pack [f] [Fill Fill_Both; Expand true];
    pack [q] [Side Side_Bottom; Fill Fill_X];
    label__configure title [Cursor (XCursor "hand2")];
    util__resizeable t
   with   
    Toplevel -> begin
	 dialog (support__new_toplevel_widget "error")
	     "Caml Browser Error"
	     ( "Cannot open module :" ^ sym)
	     (Predefined "error")
	     0
	     ["Ok"];
	 ()
	end
   | Desc_not_found -> 
      	if not silent then begin
	  dialog (support__new_toplevel_widget "error")
	      "Caml Browser Error"
	      ( sym ^ " is undefined")
	      (Predefined "error")
	      0
	      ["Ok"];
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

let global_namer g =
  g.qualid.qual ^ "__" ^ g.qualid.id
;;


let visual_type =
    visual_meta {namer = (function g -> "Type " ^ global_namer g);
      	       	 finder = (function s -> find_type_desc (str_to_qual s));
		 hyperprinter = print_type_desc}
;;

hyper_action := visual_type false
;;


let visual_constr =
    visual_meta {namer = (function g -> "Constructor " ^ global_namer g);
      	       	 finder = (function s -> find_constr_desc (str_to_qual s));
		 hyperprinter = print_constr_desc}
;;

let visual_value =
    visual_meta {namer = (function g -> "Value " ^ global_namer g);
      	       	 finder = (function s -> find_value_desc (str_to_qual s));
		 hyperprinter = print_value_desc}
;;

let visual_label =
    visual_meta {namer = (function g -> "Label " ^ global_namer g);
      	       	 finder = (function s -> find_label_desc (str_to_qual s));
		 hyperprinter = print_label_desc}
;;

let visual_module =
    visual_meta {namer = (function m -> "Module " ^ m.mod_name);
      	         finder = find_module;
		 hyperprinter = print_module}
;;

		 


let visual_search_any s =
  visual_type true s;
  visual_constr true s;
  visual_value true s;
  visual_label true s
;;


