
#open "general";;
#open "directory_and_file";;
#open "command_line";;
#open "compiler_dependant_stuff";;
#open "normalization";;
#open "matching";;
#open "output";;
#open "myTypes";;
#open "Equal";;

let right_suffix file = filename__check_suffix file ".zi";;

let expanse_file file =
  if file_exist file then
    if is_directory file then
      list_filter right_suffix (directory_content file)
    else
      [file]
  else
    let file2 = file ^ ".zi" in
      if (file_exist file2) && not (is_directory file2) then
	[file2]
      else
        (prerr_endline ("File not found : " ^ file);
         []);;

let expanse_file_list =
  flat_map
    expanse_file;;

exception Found;;

let match_less_general depth type1 ((file2, name2, e_type2, type2) as val) =
  try
    if
      type_match
        depth
        (function () -> raise Found)
        type1
	type2
    then
      [val]
    else
      []
  with
    Found ->
      print_string (string_of_value (file2, name2, e_type2));
      print_newline ();
      [];;

let match_more_general depth type1 ((file2, name2, e_type2, type2) as val) =
  try
    if
      type_match
        depth
        (function () -> raise Found)
	type2
        type1
    then
      [val]
    else
      []
  with
    Found ->
      print_string (string_of_value (file2, name2, e_type2));
      print_newline ();
      [];;

let main () =
  try
    let
      (mode, ref_type, modules, verbose, full_iso, limited_depth, max_depth,
      	 write_module_name)
    =
      read_command_line ()
    in
      output__verbose := verbose;
      output__write_module_name := write_module_name;
      normalization__full_iso := full_iso;
      let library_type = flat_map read_interface (expanse_file_list modules) in
        match mode with
	  Equality ->
	    let (_, subst, ty) = ref_type in
	      if subst > -1 then
	        (prerr_endline "`_a-variables' are not allowed in this mode";
      	       	 exit 1);
              let ref_type2 = squeeze_typ ty in
	        TypeRewrite__full_iso := full_iso;
	        do_list (filter_iso_to ref_type2) library_type
	| Less_general ->
	    let ref_type2 = normalize_entry_to_pattern ref_type in
	      let rec loop depth =
	        function
                  [] -> ()
	        | l ->
		    if (not limited_depth) || (depth <= max_depth) then
                      (if !output__verbose then
      	       	       	 (print_string "Depth ";
                          print_int depth;
                          print_newline ());
      	               loop
      	       	         (depth + 1)
                         (flat_map (match_less_general depth ref_type2) l))
      	      in
	        loop
      	       	  1
                  (map
                     (function (x, y, z) -> (x, y, z, normalize_subject z))
                     library_type)
	| More_general ->
	    (let (_, subst, _) = ref_type in
	       if subst > -1 then
	         (prerr_endline "`_a-variables' are not allowed in this mode";
      	       	  exit 1));
	    let ref_type2 = normalize_subject ref_type in
	      let rec loop depth =
	        function
                  [] -> ()
	        | l ->
		    if (not limited_depth) || (depth <= max_depth) then
                      (if !output__verbose then
      	       	       	 (print_string "Depth ";
                          print_int depth;
                          print_newline ());
      	               loop
      	       	         (depth + 1)
                         (flat_map (match_more_general depth ref_type2) l))
      	      in
	        loop
      	       	  1
                  (map
                     (function (x, y, z) ->
      	       	        (x, y, z, normalize_library_to_pattern z))
                     library_type)
	| _ -> failwith "??? Incorrect mode !!!"
  with
    Error -> exit 1;;

printexc__f main ();;
