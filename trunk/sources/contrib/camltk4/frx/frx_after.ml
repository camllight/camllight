#open "protocol";;

let idle f =
  let id = new_function_id () in
  let wrapped _ =
    clear_callback id; (* do it first in case f raises exception *)
    f() in
  hashtblc__add callback_naming_table id wrapped;
    tkDo [| TkToken "after"; TkToken "idle";
      	    TkToken ("camlcb "^ string_of_cbid id) |]
;;
