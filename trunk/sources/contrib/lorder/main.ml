#open "sys";;
#open "filename";;

(* Pieces from the compiler *)
#open "config";;
#open "misc";;
(* for these, we use the types only *)
#open "const";;
#open "reloc";;
#open "emit_phr";;

(**********************************************************************)
exception Partial 
;;
 
let map_partial f = map_aux 
  where rec map_aux = function
    [] -> []
  | (x::l) -> try let y = f x in y :: (map_aux l)
              with Partial -> (map_aux l)
 
;;

(* Make set out of list *)
let reduce l =  red l [] 
  where rec red = fun [] l -> l
      	       	    | (x::rest) l -> red rest (if memq x l then l else x::l)
;;

(***********************************************************************)
(* Find definitions for one phrase *)
let phrase_defs phr =
  it_list (fun dep (Reloc_setglobal {qual = s},_) ->
      	       	  s::dep
      	     |  dep _ -> dep)
          []
	  phr.cph_reloc
;;

(* Find out the various modules forming a library *)
let defined_library libname =
  try
    let ic = open_in_bin libname in
    let n = input_binary_int ic in
    seek_in ic n;
    let index = (input_value ic : compiled_phrase list) in
      close_in ic ;
      reduce (flat_map phrase_defs (rev index))
  with _ -> prerr_string "Using \"";
      	    prerr_string libname;
	    prerr_endline "\" as module name";
      	   [chop_suffix (basename libname) ".zo"]
;;

(* Modules to be considered as belonging to libraries *)
let libmodules = ref ([] : string list)
;;
let add_lib l =
    libmodules := (defined_library l) @ !libmodules
;;

(***********************************************************************)
(* Find dependencies for one phrase *)
let phrase_deps phr =
  it_list (fun dep (Reloc_getglobal {qual = s},_) ->
      	       	  s::dep
      	     |  dep _ -> dep)
          []
	  phr.cph_reloc
;;

(* Find dependencies for one object file *)
let file_deps filename =
  try
    let ic = open_in_bin (find_in_path filename) in
    let n = input_binary_int ic in
    seek_in ic n;
    let index = (input_value ic : compiled_phrase list) in
    let smallname = chop_suffix (basename filename) ".zo" in
      close_in ic ;
      (smallname, subtract (reduce (flat_map phrase_deps index))
                           (smallname::!libmodules))
  with 
    Cannot_find_file s -> begin 
      prerr_string "Skipping ";
      prerr_string filename;
      prerr_string " (cannot find file)\n";
      raise Partial
     end
  | Sys_error s -> begin
      prerr_string "Skipping ";
      prerr_string filename;
      prerr_string " (";
      prerr_string s;
      prerr_string ")\n";
      raise Partial
     end
;;
(**********************************************************************)
(* List of .zo files to be ordered *)
let files = ref ([]: string list)
;;

(* Do we sort *)
let sortflag = ref false
;;

(* camllorder options:
  -stdlib: sets directory of standard lib
  -lib   : add name as library file 
      	       	 or as library module
  -s     : ask for sort
*)

let main () =
  path_library := "/usr/local/lib/caml-light";
  arg__parse ["-stdlib",  arg__String (fun p -> path_library := p);
      	      "-lib",     arg__String add_lib;
	      "-I",       arg__String (fun s -> load_path := s::!load_path);
      	      "-s",       arg__Unit   (fun _ -> sortflag := true) ]
      	      (fun filename ->
      	       	 if check_suffix filename ".zo" then
      	       	    files := filename :: !files
      	       	 else failwith ("Not an object file: " ^ filename));
  (* there is always this one (? profiler ?)*)
  add_lib (concat !path_library "stdlib.zo");
  let deps = map_partial file_deps !files in
    if !sortflag then begin
       let order = tsort__new () in
       	 (* Add elements (reflexivity) *)
         do_list (function filename ->
	            tsort__add_element order 
      	       	       (chop_suffix (basename filename) ".zo"))
                 !files;
	 (* Add ordering relations *)
       	 do_list (function (m,dep) ->
	             do_list (fun d -> tsort__add_relation order (d,m)) dep)
		 deps;
	 (* Output the sort *)
         do_list (function f -> print_string f; print_string ".zo ")
	         (tsort__sort order);
       print_newline()
    end else
      	do_list (fun 
      	       	  (f,[]) -> 
		       print_string f; print_string ".zo ";
		       print_string f; print_string ".zo";
		       print_newline()
      	       	| (f,ds) ->
	           do_list (fun d ->
			      print_string d; print_string ".zo ";
		              print_string f; print_string ".zo";
			      print_newline())
			   ds)
		deps
   ;;

printexc__f main ()
;;

