(****************** Tools for Unix *****************************************)


#open "misc";;
#open "unix";;
#open "primitives";;

(*** Convert a socket name into a socket address. ***)
let convert_adress address =
  try
    let n = string_pos address `:` in
      let host = sub_string address 0 (n - 1)
      and port = sub_string address (n + 1) (string_length address)
      in
        (PF_INET,
	 ADDR_INET
	   ((try inet_addr_of_string host with Failure _ ->
	       try (gethostbyname host).h_addr_list.(0) with Not_found ->
	         prerr_endline ("Unknown host : " ^ host);
		 failwith "Can't convert address"),
	    (try int_of_string port with Failure _ ->
	       prerr_endline "The port number should be an integer";
	       failwith "Can't convert address")))
  with Not_found ->
      (PF_UNIX, ADDR_UNIX address);;

(*** Report an unix error. ***)
let report_error = function
    Unix_error (err, fun_name, arg) ->
     prerr_string "Unix error : '";
     prerr_string fun_name;
     prerr_string "' failed";
     if string_length arg > 0 then
       (prerr_string " on '";
        prerr_string arg;
        prerr_string "'");
     prerr_string " : ";
     prerr_endline (error_message err)
  | _ -> fatal_error "report_error: not an Unix error";;

(* Find program `name' in `PATH'. *)
(* Return the full path if found. *)
(* Raise `Not_found' otherwise. *)
let search_in_path name =
  let check name =
    try access name [X_OK]; name with Unix_error _ -> raise Not_found
  in
    if (try let _ = string_pos name `/` in true with Not_found -> false) then
      check name
    else
      let path = sys__getenv "PATH" in
        let length = string_length path in
          let rec traverse pointer =
            if (pointer >= length) || (nth_char path pointer = `:`) then
              pointer
            else
              traverse (pointer + 1)
          in
            let rec find pos =
              let pos2 = traverse pos in
                let directory = (sub_string path pos (pos2 - pos)) in
	          let fullname =
	            if directory = "" then
	              name
                    else
	              directory ^ "/" ^ name
                  in
                    try check fullname with
                      Not_found ->
                        if pos2 < length then
                          find (pos2 + 1)
                        else
	                  raise Not_found
          in
            find 0;;

(* Expand a path. *)
(* ### path -> path' *)
let rec expand_path ch =
  let rec subst_variable ch =
    try
      let pos = string_pos ch `$` in
        if (pos + 1 < string_length ch) && (nth_char ch (pos + 1) = `$`) then
      	  (sub_string ch 0 (pos + 1))
	    ^ (subst_variable
      	         (sub_string ch (pos + 2) (string_length ch - pos - 2)))
        else
          (sub_string ch 0 pos)
            ^ (subst2 (sub_string ch (pos + 1) (string_length ch - pos - 1)))
    with Not_found ->
      ch
  and subst2 ch =
    let suiv =
      let i = ref 0
      and stream = stream_of_string ch
      in
        while
          (function
             [< '`a`..`z` >] -> true
           | [< '`A`..`Z` >] -> true
           | [< '`0`..`9` >] -> true
           | [< '`_` >] -> true
           | [< '_ >] -> false
           | [< >] -> false) stream
        do i := !i + 1 done;
        !i
    in (sys__getenv (sub_string ch 0 suiv))
       ^ (subst_variable (sub_string ch suiv (string_length ch - suiv)))
  in
    let ch = subst_variable ch in
      let concat_root nom ch2 =
        try filename__concat (getpwnam nom).pw_dir ch2
        with Not_found ->
          raise (Invalid_argument "expand_path")
      in
        if (nth_char ch 0) = `~` then
	  try
            match string_pos ch `/` with
              1 ->
                (let tail = sub_string ch 2 (string_length ch - 2)
                 in
                   try filename__concat (sys__getenv "HOME") tail
                   with Not_found ->
                     concat_root (sys__getenv "LOGNAME") tail)
            |  n -> concat_root
                      (sub_string ch 1 (n - 1))
                      (sub_string ch (n + 1) (string_length ch - n - 1))
          with
	    Not_found ->
              expand_path (ch ^ "/")
        else ch;;
