#open "slavetop";;
#open "top";;
#open "unix";;

let parse_options () =
  arg__parse [ "-toplevel",
                arg__String (fun s -> toplevel_bin := s)]
	      (fun _ -> ())
;;


let main () =
  parse_options();
  try 
    setup();
    tk__MainLoop()
  with Unix_error(err, fun_name, arg)  as exc ->
      begin (* from handle_unix_error *)
      prerr_string "Unix_error : \"";
      prerr_string fun_name;
      prerr_string "\" failed";
      if string_length arg > 0 then begin
	prerr_string " on \"";
	prerr_string arg;
	prerr_string "\""
      end;
      prerr_string ": ";
      prerr_endline (error_message err);
      raise exc
      end
;;

printexc__f main ();;
