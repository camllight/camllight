#open "tk";;




let top = OpenTk() in
  let files = listbox__create top 
      	       	[Relief Raised; Borderwidth (Pixels 2)]
  and scroll = scrollbar__create top [] in
    listbox__configure files
      	    [YScrollCommand (scrollbar__set scroll)];
    scrollbar__configure scroll
      	    [Slidecommand (fun n -> listbox__yview files (Number n))];
  pack [files] [Side Side_Left];
  pack [scroll] [Side Side_Right; Fill Fill_Y];
  (* replaced [glob *] by arbitrary strings *)
  listbox__insert files End 
    ["this";"is";"a";"test";"of";"linked";"listbox";"and";"scrollbar";
      "widgets"];
  MainLoop()
;;


