#open "tk";;
#open "lexing";;
#open "lexer";;
#open "emacsbindings";;
#open "slavetop";;
#open "unix";;
#open "sessions";;

let setup () =
  let top = OpenTk() in

  (* Get our slave toplevel *)
  let toplevel_in, toplevel_read, toplevel_write, toplevel_pid =
     slave_toplevel() in

  let bye () =
    toplevel_write "quit();;\n";
    CloseTk();
    exit 0  in
  
  (* Title *)
  let title = 
     label__create top [Text "Caml Light Toplevel"; Relief Sunken] in

  (* Menubar *)
  (* Menus *)
  let menu_frame = frame__create top [] in
  let file_menu = menubutton__create menu_frame [Text "File"] in
  let file_menu_items = menu__create file_menu [] in
    menu__add_command file_menu_items [Label "Load Session"; State Disabled];
    menu__add_command file_menu_items [Label "Save Session"; State Disabled];
    menu__add_separator file_menu_items;
    menu__add_command file_menu_items 
      	[Label "Include"; Command (include toplevel_write)];
    menu__add_command file_menu_items 
      	[Label "Load Object"; Command (load_object toplevel_write)];
    menu__add_separator file_menu_items;
    menu__add_command file_menu_items [Label "Quit"; Command bye];
    menubutton__configure file_menu [Menu file_menu_items];

  let edit_menu = menubutton__create menu_frame [Text "Edit"] in
  let edit_menu_items = menu__create edit_menu [] in
    menu__add_command edit_menu_items [Label "Cut"];
    menu__add_command edit_menu_items [Label "Copy"; State Disabled];
    menu__add_command edit_menu_items [Label "Paste"];
    menubutton__configure edit_menu [Menu edit_menu_items];
    menubar__set menu_frame [file_menu; edit_menu];

  let break_button = button__create menu_frame [Text "Break"] in
  (* Interrupting the slave toplevel *)
    button__configure break_button 
      	       	 [Command (function () -> kill toplevel_pid SIGINT)];
  (* Debugging *)
  let debug_button = 
     button__create menu_frame 
      	       	[Text "Debug"; 
      	       	 Command (fun () -> protocol__debug := not !protocol__debug)] in

  (* Text zone and associated scrollbar *)
  let toplevel_frame = frame__create top [Relief Sunken; Borderwidth (Pixels 2)] in
  let toplevel_text = text__create toplevel_frame [Wrap WrapNone; ExportSelection true] in
  let toplevel_scroll = scrollbar__create toplevel_frame [] in

  (* Link text and scrollbar *)
  scrollbar__configure toplevel_scroll 
                    [Slidecommand (text__yview_line toplevel_text)];
  text__configure toplevel_text 
                    [YScrollCommand (scrollbar__set toplevel_scroll)];

  (* Another text zone for toplevel logs and errors *)
  let log_frame = frame__create top [Relief Sunken; Borderwidth (Pixels 2)] in
  let log_text = text__create log_frame [TextHeight 10;Wrap WrapNone ] in
  let log_scroll = scrollbar__create log_frame [] in
     scrollbar__configure log_scroll [Slidecommand (text__yview_line log_text)];
     text__configure log_text [YScrollCommand (scrollbar__set log_scroll)];


  (* Async read of toplevel results and redisplay *)
  let callback_toplevel () =
    let str = toplevel_read () in
    text__insert log_text (TextIndex(TI_End,[])) str;
    text__yview_pickplace log_text (TextIndex(TI_Mark "insert",[])) in

  tk__add_fileinput toplevel_in callback_toplevel;

  (**********************************************************************)
  (* Management of toplevel text 					*)
  (* Emacs like bindings *)
  emacs_text toplevel_text;
  (* Cut/Copy/Paste Menus *)
  menu__entryconfigure_command edit_menu_items (MI_Number 0)
      [Command (wrap_kill_region_redisplay kill_region toplevel_text)];
  (* Copy does nothing ... *)
  menu__entryconfigure_command edit_menu_items (MI_Number 2)
      [Command (wrap_noedit_redisplay yank toplevel_text)];
  

  (***** Special annotation for keywords *****)
  text__tag_configure toplevel_text "keyword" [Foreground Blue];

  let bold_keywords start s =
    let l = create_lexer_string s in
    let rec next_token () =
       match Main l with
	 Keyword (_,i,j) -> text__tag_add toplevel_text "keyword"
				 (char_index start i)
				 (char_index start j) ;
			    next_token()

       |  Dontcare -> next_token ()
       |  EOF -> () in
     next_token ()  in

   (***** The fixed part *****)
    text__tag_configure toplevel_text "readonly" [Background White];
    text__tag_lower toplevel_text"readonly" "sel";

   (* End of phrase *)
   let phrase_start = ref (TI_LineChar(0,0)) in

   let end_phrase = fun ev ->
       (* second semi-colon is eaten *)
       text__insert toplevel_text (TextIndex(TI_End,[])) ";"; 
       let start_of_phrase = !phrase_start
       and end_of_phrase = text__index toplevel_text (TextIndex(TI_End,[])) in
	   phrase_start := end_of_phrase;
       let s = text__get toplevel_text (TextIndex (start_of_phrase,[]))
				       (TextIndex (end_of_phrase,[])) in
	   toplevel_write s;
	   text__insert toplevel_text (TextIndex(TI_End,[])) "\n"; 
	   text__tag_add toplevel_text "readonly"
		 (TextIndex (start_of_phrase,[])) (line_end end_of_phrase);

	   bold_keywords start_of_phrase s
     in

     tk__bind toplevel_text [[],XKey "semicolon" ; [],XKey "semicolon"]
      	                    (BindSet([], end_phrase));


      pack [title;menu_frame;toplevel_frame;log_frame] [Fill Fill_X];

      pack [file_menu; edit_menu] [Side Side_Left; Fill Fill_Y];
      pack [break_button; debug_button] [Side Side_Right; Fill Fill_Y];

      pack [toplevel_scroll] [Side Side_Right; Fill Fill_Y];
      pack [toplevel_text] [Fill Fill_Both; Expand true];

      pack [log_scroll] [Side Side_Right; Fill Fill_Y];
      pack [log_text] [Fill Fill_Both]
;;
    

