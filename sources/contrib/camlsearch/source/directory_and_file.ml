#open "unix";;

let directory_content directory_name =
    let directory = opendir directory_name in
      let rec content () =
        try
      	  let entry = readdir directory in
	    (filename__concat directory_name entry)::(content ())
        with
      	  End_of_file -> []
      in
        let lst = content () in
      	  closedir directory; lst;;

let is_directory file = (stat file).st_kind = S_DIR;;

let file_exist file = try let _ = stat file in true with _ -> false;;
