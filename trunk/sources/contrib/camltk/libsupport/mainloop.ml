#open "unix";;
let NextCallback () =
  let str = GetTkToken !PipeTkCallB in
    try (hashtbl__find callback_table str) () 
    with exc -> CloseTk(); raise exc
;;

(* Support for fileinput callbacks *)
let channel_callbacks = 
  (hashtbl__new 11 : (file_descr, (unit -> unit)) hashtbl__t);;
let channels  = ref ([] : file_descr list)
;;

let add_fileinput fd cb =
  channels := fd :: !channels;
  hashtbl__add channel_callbacks fd cb
;;

let remove_fileinput fd = 
  channels := except fd !channels;
  hashtbl__remove channel_callbacks fd
;;

(* Redefine OpenTk with fileinput support *)
let OpenTk () = 
  let top = OpenTk () in
    add_fileinput !protocol__PipeTkCallB NextCallback;
    (top : Widget)
;;
let OpenTkClass s = 
  let top = OpenTkClass s in
    add_fileinput !protocol__PipeTkCallB NextCallback;
    (top : Widget)
;;

(* Redefine CloseTk so it appears in module tk *)
let CloseTk () = 
  remove_fileinput !protocol__PipeTkCallB;
  CloseTk()
;;

let NextEvent () =
    let (ins,_,_) = select !channels [] [] (-1.0) in
       do_list (fun fd -> (hashtbl__find channel_callbacks fd) ())
               ins
;;

let MainLoop () =
  while true do NextEvent() done
;;

  
