(**************************** To print values *******************)

#open "misc";;
#open "pr_type";;
#open "string_of_type";;
#open "const";;
#open "globals";;
#open "builtins";;
#open "types";;
#open "modules";;
#open "symtable";;
#open "primitives";;
#open "value";;
#open "pure_queue";;
#open "debugger_config";;

(*** Utilities.***)

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

(*** Formatter.***)

type STRUCTURE =
    Future of unit -> STRUCTURE
  | Leaf   of string
  | Tagged of string * bool * STRUCTURE ref
  | Set    of SET
and SET =
  {Head      : string;
   Separator : string;
   Tail      : string;
   Content   : STRUCTURE ref list};;

type NODE =
  {	   Max_width : int;		(* Optimistic bound. *)
      	   Min_width : int;		(* Pessimistic bound. *)
   mutable Width     : int;
   mutable Height    : int;
   mutable Complex   : bool;
   mutable Split     : bool;
      	   Kind      : STRUCTURE ref;
   mutable Parent    : NODE OPTION;
   mutable Childs    : NODE list};;

let new_node max_width min_width parent struct =
  {Kind = struct; Parent = parent; Childs = [];
   Max_width = max_width; Min_width = min_width;
   Width = 0; Height = 0;
   Complex = false; Split = false};;

let expand =
  function
    ref (Future f) as struct ->
      let expanded_struct = f () in
      	struct := expanded_struct;
	expanded_struct
  | ref x ->
      x;;

let hidden = "***";;

let hidden_node node =
  node.Kind := Leaf hidden;
  node.Childs <- [];
  node.Width <- 3;
  node.Height <- 1;
  node.Complex <- false;
  node.Split <- false;;

let handle_node queue node =
  match expand node.Kind with
    Leaf s ->
      node.Childs <- [];
      node.Width <- string_length s;
      node.Height <- 1;
      node.Complex <- false;
      node.Split <- false;
      queue
  | Tagged (s, _, t) ->
      if (node.Max_width <= 2) or (node.Max_width < string_length s) then
        (node.Childs <- [];		(* Partially hidden. *)
         node.Width <- string_length s;
         node.Height <- 1;
      	 node.Complex <- false;
      	 node.Split <- false;
	 queue)
      else
        let child =
          new_node (node.Max_width - 2) (node.Min_width - 2) (Some node) t
        in
          node.Childs <- [child];
          node.Width <- string_length s + 1;
          node.Height <- 1;
      	  node.Complex <- false;
      	  node.Split <- false;
	  insert queue child
  | Set {Head = head; Separator = sep; Tail = tail; Content = l} ->
      if (node.Max_width <= string_length head) then
        (node.Childs <- [];
         node.Width <- string_length head;
         node.Height <- 1;
      	 node.Complex <- true;
      	 node.Split <- false;
	 queue)
      else
        let childs =
	  let max_width =
            node.Max_width - string_length head
	  and min_width =
            node.Min_width - string_length head
              - max (string_length sep) (string_length tail)
          in
            map (new_node max_width min_width (Some node)) l
      	and width =
	  string_length (head ^ tail)
      	    + (if l = [] then 0
      	       else (list_length l - 1) * (string_length sep + 2))
        in
	  if width <= node.Min_width then
            (node.Childs <- childs;
             node.Width <- width;
             node.Height <- 1;
      	     node.Complex <- true;
      	     node.Split <- false)
          else
	    (node.Childs <- childs;
	     node.Width <- string_length (head ^ tail) + 1;
	     node.Height <- list_length l;
	     node.Complex <- true;
	     node.Split <- true);
	  it_list insert queue childs
  | Future _ ->
      failwith "????handle_node";;

let rec update_tree =
  function
    {Parent = None} as node ->
      node
  | {Parent = Some parent} as node ->
      match parent with
	{Kind = ref (Tagged (s, _, _))} ->
	  let width0 = string_length s + 1 + node.Width in
	    let (width, height, split) =
      	      if width0 > parent.Min_width then
		(node.Width + 2, node.Height + 1, true)
	      else
		(width0, node.Height, false)
	    in
	      parent.Width <- width;
	      parent.Height <- height;
      	      parent.Complex <- node.Complex;
      	      parent.Split <- split;
	      update_tree parent
      | {Kind = ref (Set {Head = head;
      	       	       	  Separator = separator;
      	       	       	  Tail = tail;
      	       	       	  Content = l});
      	 Childs = childs} ->
          let (width, height, split) =
            let complex () =
      	      exists (function p -> p.Complex or (p.Height > 1)) childs
            and width =
              let sep_size = string_length separator + 1 in
                list_it
                  (fun p n -> n + p.Width + sep_size)
                  childs
                  (string_length (head ^ tail) - sep_size)
            in
              if complex () or (width > parent.Min_width) then
		let rec size =
		  function
		    [] -> 0
		  | [a] -> a.Width + string_length tail
                  | a::l ->
		      let s =
      	       	        a.Width + string_length separator
		      and t =
			size l
		      in
			if s > t then s else t
		in
                  (string_length head + size childs,
                   list_it (fun {Height = h} n -> h + n) childs 0,
      	       	   true)
              else
                (width, 1, false)
          in
  	    parent.Width <- width;
  	    parent.Height <- height;
      	    parent.Complex <- true;
      	    parent.Split <- split;
	    update_tree parent;;

let traverse max_height tree =
  let rec trav queue =
    let (node, new_queue) = remove queue in
      let new_new_queue = handle_node new_queue node in
        update_tree node;
	if tree.Height <= max_height then
          trav new_new_queue;
        if (node.Kind <> ref (Leaf hidden))
             & (match node.Kind with
      	          ref (Tagged (_, false, _)) -> false
      	        | _ -> true)
        then
	  (hidden_node node;
           update_tree node;
      	   if tree.Height <= max_height then
             trav new_queue)
  in
    trav;;

let buffer = ref "";;

let pr_string s =
  buffer := !buffer ^ s;;

let pr_endline s =
  pr_string s;
  if string_length !buffer > !screen_width then
    buffer := sub_string !buffer 0 (!screen_width - 3) ^ "...";
  print_endline !buffer;
  buffer := "";;

let rec fill length =
  pr_string (make_string length ` `);;

let rec print_struct offset =
  function
    {Kind = ref (Leaf s)} ->
      pr_string s
  | {Kind = ref (Tagged (header, _, _)); Childs = [child]; Split = split} ->
      if split then
        (pr_endline header;
	 fill (offset + 2);
	 print_struct (offset + 2) child)
      else
        (pr_string header;
	 pr_string " ";
	 print_struct (offset + string_length header + 1) child)
  | {Kind = ref (Tagged (header, _, _))} ->
					(* Truncated header. *)
      pr_string header
  | {Kind = ref (Set {Head = h; Separator = s; Tail = t; Content = c});
     Childs = childs;
     Split = split} ->
      if split then
      	(pr_string h;
	 let new_offset = offset + string_length h in
	   let rec print =
	     function
	       [] ->
	         failwith "???print_struct"
             | [a] ->
	         print_struct new_offset a;
		 pr_string t
	     | a::l ->
	         print_struct new_offset a;
		 pr_endline s;
		 fill new_offset;
		 print l
      	   in
      	     print childs)
      else
      	(pr_string h;
	 let rec print =
	   function
	     [] ->
	       pr_string t
	   | [a] ->
	       print_struct 0 a;
	       pr_string t
           | a::l ->
	       print_struct 0 a;
	       pr_string s;
	       pr_string " ";
	       print l
         in
	   print childs);;

let print_structure struct max_height =
  let tree = new_node !screen_width !screen_width None struct in
    try
      traverse (max max_height 1) tree (insert empty tree);
      failwith "???print_structure"
    with
      Empty ->
        print_struct 0 tree;
        pr_endline "";;

(*** Printer. ***)

(* Printers for simple types *)
let printers = ref [
  constr_type_int,
    (fun x -> string_of_int (int_of_value x));
  constr_type_float,
    (fun x -> string_of_float (float_of_value x));
  constr_type_char,
    (fun x -> "`" ^ (char_for_read (char_of_value x)) ^ "`");
  constr_type_string,
   (fun x -> "\"" ^ (string_for_read (string_of_value x)) ^ "\"")
];;

let rec string_of_val prio obj ty =
  match (type_repr ty).typ_desc with
    Tvar _ ->
      Leaf "<poly>"
  | Tarrow(ty1, ty2) ->
      Leaf "<fun>"
  | Tproduct(ty_list) ->
      string_of_val_list 1 obj ty_list
  | Tconstr(cstr, ty_list) ->
      if same_type_constr cstr constr_type_list then
        (let ty_arg = 
           match ty_list with [ty] -> ty | _ -> fatal_error "string_of_val (list)"
         in
	   let rec elements n cons =
	     if value_tag cons == 0 then
	       []
	     else if n = 0 then
	       [ref (Leaf "...")]
             else
	       (ref
      	       	  (Future
      	       	     (function () ->
      	       	        string_of_val
                          0
      	       	       	  (value_nth_field cons 0)
      	       	       	  ty_arg)))
	         ::(elements (n - 1) (value_nth_field cons 1))
	   in
	     Set {Head = "[";
      	       	  Separator = ";";
      	       	  Tail = "]";
      	       	  Content = elements 5 obj})
        else if same_type_constr cstr constr_type_vect then
          let ty_arg =
            match ty_list with [ty] -> ty | _ -> fatal_error "string_of_val (vect)"
          in
	    let rec elements n =
	      if n < 0 then
	        []
              else
	        (ref
		   (Future
		      (function () ->
	                 string_of_val
                           0
      	       	       	   (value_nth_field obj n)
      	       	       	   ty_arg)))
	          ::(elements (n - 1))
	    in
	      let size = value_size obj in
	        let content =
		  rev
	            (if size > 5 then
		       (ref (Leaf "..."))::(elements 4)
		     else
		       elements size)
		in
	     Set {Head = "[|";
      	       	  Separator = ";";
      	       	  Tail = "|]";
      	       	  Content = elements 5}
        else
          try
            let rec find_printer = function
              [] ->
                raise Not_found
            | (cstr', f) :: rest ->
                if same_type_constr cstr cstr'
                then f obj
                else find_printer rest
            in
              Leaf (find_printer !printers)
          with Not_found ->
            match cstr.info.ty_abbr with
              Tabbrev(params, body) ->
                string_of_val
                  prio
                  obj
                  (expand_abbrev params body ty_list)
            | _ ->
                string_of_concrete_type prio obj cstr ty ty_list

and string_of_concrete_type prio obj cstr ty ty_list =
  let typ_descr =
    type_descr_of_type_constr cstr in
  match typ_descr.info.ty_desc with
    Abstract_type ->
      Leaf "<abstract>"
  | Variant_type constr_list ->
      let tag = value_tag obj in
      begin try
        let constr = 
          if same_type_constr cstr constr_type_exn
          then find_exception tag
          else find_constr tag constr_list in
        let (ty_res, ty_arg) =
          type_pair_instance (constr.info.cs_res, constr.info.cs_arg) in
        types__filter (ty_res, ty);
        match constr.info.cs_kind with
          Constr_constant ->
            Leaf (string_of_constr constr)
        | Constr_regular ->
	    let text () =
	      Tagged
      	       	(string_of_constr constr,
		 true,
		 ref
      	       	   (Future
		      (function () ->
                         string_of_val
                           2
                           (value_nth_field obj 0)
                           ty_arg)))
            in
	      if prio > 1 then
	        Set {Head = "("; Separator = ""; Tail = ")";
      	       	     Content = [ref (Future text)]}
              else
	        text ()
        | Constr_superfluous n ->
	    let text () =
	      Tagged
      	       	(string_of_constr constr,
		 true,
      	       	 ref
		   (Future
		      (function () ->
                         string_of_val_list
                           1
                           obj
                           (filter_product n ty_arg))))
            in
	      if prio > 1 then
	        Set {Head = "("; Separator = ""; Tail = ")";
      	       	     Content = [ref (Future text)]}
              else
	        text ()
      with
        Not_found ->
          Leaf "<local exception>"
      | Unify ->
          fatal_error "string_of_val: types should match"
      end
  | Record_type label_list ->
      let string_of_field lbl =
      	ref
      	  (Tagged
	     (string_of_label lbl ^ " =",
	      false,
	      ref
	        (Future
	           (function () ->
                      let (ty_res, ty_arg) =
                        type_pair_instance (lbl.info.lbl_res, lbl.info.lbl_arg)
                      in
                        (try types__filter (ty_res, ty) with Unify ->
                           fatal_error "string_of_val: types should match");
                         string_of_val
                           0
                           (value_nth_field obj lbl.info.lbl_pos)
      	                   ty_arg))))
      in
      	Set
      	  {Head = "{"; Separator = ";"; Tail = "}";
	   Content = map string_of_field label_list}
  | Abbrev_type(_,_) ->
      fatal_error "string_of_val: abbrev type"

and string_of_val_list prio obj ty_list =
  let rec string_of_list i =
    function
      [] ->
        fatal_error "string_of_val_list"
    | [ty] ->
      	[ref
           (Future
      	      (function () ->
      	         string_of_val prio (value_nth_field obj i) ty))]
    | ty::ty_list ->
        (ref
      	   (Future
              (function () ->
      	         string_of_val prio (value_nth_field obj i) ty)))
          ::(string_of_list (succ i) ty_list)
  in
    Set {Head = "(";
      	 Separator = ",";
      	 Tail = ")";
      	 Content = string_of_list 0 ty_list};;

let print_value header obj ty =
  let rec print_header header height =
    if string_length header > !screen_width then
      let end_of_line =
        let rec find_space pos n =
          if (n = 0) or (pos = 0) then
            !screen_width
          else if nth_char header (pos - 1) = ` ` then
            pos
          else
            find_space (pos - 1) (n - 1)
        in
          find_space !screen_width 10
      in
        print_endline (sub_string header 0 end_of_line);
        print_header
          (sub_string header
             end_of_line
             (string_length header - end_of_line))
	  (height + 1)
    else
      (header, height)
  in
    let (last, height) = print_header header 0 in
      print_structure
        (ref
           (Tagged
              (last,
      	       true,
               ref (Future (function () -> string_of_val 0 obj ty)))))
        (!max_height - height);;
