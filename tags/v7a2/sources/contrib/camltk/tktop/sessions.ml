(* Loading of sessions: *)
(*
let load () =
  let filename = contrib__FSBox() in
  if filename <> "" then
    toplevel_write ("include\""^filename^"\";;")
*)  


(* Include *)
let include w () =
  let filename = contrib__FSBox() in
  if filename <> "" then
    w ("include\""^filename^"\";;")
;;

let load_object w () =
  let filename = contrib__FSBox() in
  if filename <> "" then
    w ("load_object\""^filename^"\";;")
;;
