(****************** Tools for Unix *****************************************)


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
let report_error (Unix_error (err, fun_name, arg)) =
  prerr_string "Unix error : '";
  prerr_string fun_name;
  prerr_string "' failed";
  if string_length arg > 0 then
    (prerr_string " on '";
     prerr_string arg;
     prerr_string "'");
  prerr_string " : ";
  prerr_endline (error_message err);;
