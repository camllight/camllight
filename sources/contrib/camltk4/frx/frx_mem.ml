(* Memory gauge *)
#open "tk";;
#open "gc";;
#open "frx_misc";;

let inited = ref None
;;
let w = ref 300
;;
let delay = ref 5 (* in seconds *)
;;
let wordsize = (* officially approved *)
  if 1 lsl 31 = 0 then 4 else 8
;;

let init () = 
  let top = toplevelw__create support__default_toplevel_widget [Class "CamlGC"] in
  let name = tk__appname_get () in
    wm__title_set top (name ^ " Memory Gauge");
    wm__withdraw top;
    inited := Some top;
    (* this should be executed before the internal "all" binding *)
    bind top [[], Destroy] (BindSet ([], (fun _ -> inited := None)));
    let fminors = frame__create top [] in
      let lminors = label__create fminors [Text "Minor collections"]
      and vminors = label__create fminors [] in
      pack [lminors][Side Side_Left];
      pack [vminors][Side Side_Right; Fill Fill_X; Expand true];
    let fmajors = frame__create top [] in
      let lmajors = label__create fmajors [Text "Major collections"]
      and vmajors = label__create fmajors [] in
      pack [lmajors][Side Side_Left];
      pack [vmajors][Side Side_Right; Fill Fill_X; Expand true];
    let fsize = frame__create top [] in
      let lsize = label__create fsize [Text "Heap size (bytes)"]
      and vsize = label__create fsize [] in
      pack [lsize][Side Side_Left];
      pack [vsize][Side Side_Right; Fill Fill_X; Expand true];
    let fheap = frame__create top [Width (Pixels !w); Height (Pixels 10)] in
    let flive = frame__create fheap [Background Red]
    and ffree = frame__create fheap [Background Green]
    and fdead = frame__create fheap [Background Black] in
      pack [fminors; fmajors; fsize; fheap][Fill Fill_X];

    let display () =
      let st = gc__stat() in
       label__configure vminors [Text (string_of_int st.minor_collections)];
       label__configure vmajors [Text (string_of_int st.major_collections)];
       label__configure vsize [Text (string_of_int (wordsize * st.heap_words))];
       let liver = (float st.live_words) /. (float st.heap_words)
       and freer = (float st.free_words) /. (float st.heap_words) in
       place__configure flive [X (Pixels 0); Y (Pixels 0);
			      RelWidth liver; RelHeight 1.0];
       place__configure ffree [RelX liver; Y (Pixels 0);
			      RelWidth freer; RelHeight 1.0];
       place__configure fdead [RelX (liver +. freer); Y (Pixels 0);
			      RelWidth (1.0 -. freer -. liver); RelHeight 1.0]

    in
    let rec tim () =
      if winfo__exists top then begin
      	display();
	let _ = add_timer (!delay * 1000) tim in
	()
      end
    in
    tim()
;;


let rec f () =
  match !inited with
    Some w -> wm__deiconify w
  | None -> init (); f()
;;
