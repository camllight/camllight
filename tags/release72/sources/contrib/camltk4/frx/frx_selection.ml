(* A selection handler *)
#open "support";;
#open "tk";;
#open "protocol";;
#open "frx_misc";;

let frame = ref None
;;
let selection = ref ""
;;
let read ofs n =
  let res =
    if ofs < 0 then ""
    else if ofs + n > string_length !selection
    then sub_string !selection ofs (string_length !selection - ofs)
    else sub_string !selection ofs n in
   tkreturn res
;;
(* As long as we don't loose the selection, we keep the widget *)
(* Calling this function means that we own the selection       *)
(* When we loose the selection, both cb are destroyed *)
let own () =
  match !frame with
    None ->
      let f = frame__create_named default_toplevel_widget "frx_selection" [] in
       let lost () = selection := ""; destroy f; frame := None in
       selection__own_set [Selection "PRIMARY"; LostCommand lost] f;
       selection__handle_set [Selection "PRIMARY"; ICCCMType "STRING"] f read;
       frame := Some f
  | Some f -> ()
;;
let set s = own(); selection := s
;;
