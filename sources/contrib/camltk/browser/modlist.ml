#open"unix";;
#open"filename";;

let modules_of_directory dir =
  let modules = ref (set__empty compare_strings) in
  begin try
    let d = opendir dir in
    try
      while true do 
	let f = readdir d in
	  if check_suffix f ".zi" then 
      	     modules := set__add (chop_suffix f ".zi") !modules
	done
      with 
	End_of_file -> closedir d
  with
    Unix_error (_,s,s') -> 
        printf__eprintf "unix error %s %s\n" s s'; flush std_err
  end;
  !modules
;;

let sources_of_directory dir =
  let files = ref (set__empty compare_strings) in
  begin try
    let d = opendir dir in
    try
      while true do 
	let f = readdir d in
	  if check_suffix f ".ml" || check_suffix f ".mli" then 
      	     files := set__add f !files
	done
      with 
	End_of_file -> closedir d
  with
    Unix_error (_,s,s') -> 
        printf__eprintf "unix error %s %s\n" s s'; flush std_err
  end;
  !files
;;

let files_of_path files_of_d path =
  let modset = 
   it_list (fun accu dir ->
	     set__union accu (files_of_d dir))
	   (set__empty compare_strings) 
	   ("." :: path) in
  sort__sort le_string (set__elements modset)
;;

let modules_of_path = files_of_path modules_of_directory
and sources_of_path = files_of_path sources_of_directory
;;
