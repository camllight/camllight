let NextEvent () =
  let str = GetTkToken !PipeTkCallB in
    try (hashtbl__find callback_table str) () 
    with exc -> CloseTk(); raise exc
;;

let MainLoop () =
  while true do
    NextEvent()
  done
;;

  
