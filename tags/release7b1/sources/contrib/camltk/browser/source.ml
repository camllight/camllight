(* Display an ML source in a text widget *)
#open "misc";;
#open "lexer";;
#open "lexing";;
#open "tk";;
#open "visual";;

let display_source t start s =
  text__insert t (TextIndex (start, [])) s;
  text__configure t [State Disabled];
  text__tag_configure t "keyword" [Underline true];
  text__tag_configure t "comment" [Foreground Red];
  let l = create_lexer_string s in
  let rec next_token () =
    match main l with
      Keyword(_,i,j) -> 
      	text__tag_add t "keyword"
	   (TextIndex (start, [CharOffset i]))
	   (TextIndex (start, [CharOffset j]));
        next_token()
   |  Commentaire(i,j) ->
      	text__tag_add t "comment"
	   (TextIndex (start, [CharOffset i]))
	   (TextIndex (start, [CharOffset j]));
        next_token()
   |  Dontcare -> next_token()
   | EOF -> () in
  next_token()
;;


let load_file filename = 
  let real_name = find_in_path filename in
  let ic = open_in_bin real_name in
  let size = in_channel_length ic in
  let s = create_string size in
    really_input ic s 0 size;
    close_in ic;
    s
;;    

let new_file_top = 
  let cnter = ref 0 in
  function () ->
   incr cnter; "file" ^ string_of_int !cnter
;;

(* Almost like visual/hypertext *)
let display_file filename =
  try
     let s = load_file filename in
     let t = toplevelw__create 
		(support__new_toplevel_widget (new_file_top())) [] in
     let title =
       label__create t [Text filename; Relief Raised] in
     pack [title] [Fill Fill_X];
     label__configure title [Cursor (XCursor "watch")];
     update();
     let f = frame__create t [] in
     let tx = text__create f [] in
       display_source tx (TI_LineChar(0,0)) s;      
     let sb = scrollbar__create f [] in
       util__scroll_text_link sb tx;
       pack [tx] [Side Side_Left; Fill Fill_Both; Expand true];
       pack [sb] [Side Side_Left; Fill Fill_Y];
       util__navigation_keys tx sb;
     let q = 
       button__create t [Text "Ok"; Relief Raised; 
			 Command (fun _ -> destroy t)] in

       pack [f] [Fill Fill_Both; Expand true];
       pack [q] [Side Side_Bottom; Fill Fill_X];
       label__configure title [Cursor (XCursor "hand2")];
       bind tx [[Any],XKey "Escape"] 
      	     (BindSet([], (fun _ -> button__invoke q)));
	     
     let get_current_anchor () =
       let b = text__index tx (TextIndex (TI_Mark "current", [WordStart]))
       and e = text__index tx (TextIndex (TI_Mark "current", [WordEnd])) in
      	text__get tx (TextIndex(b,[]))  (TextIndex(e,[])) in

     bind tx [[Double], WhatButton 1] 
      (BindExtend ([], fun _ -> visual_search_any (get_current_anchor())));

       util__resizeable t
  with 
    Cannot_find_file filename ->
      begin dialog (support__new_toplevel_widget "error")
      	"Caml Browser Error"
	("Cannot open " ^ filename )
	(Predefined "error") 0 ["Ok"]; ()
      end
;;
