#open "tk";;

(* Remove default key bindings *)
(* (keep mouse though) *)
let text_clean_default t =
  bind t [[Any],KeyPress] BindRemove;
  bind t [[], (XKey "Return")] BindRemove;
  bind t [[], (XKey "BackSpace")] BindRemove;
  bind t [[], (XKey "Delete")] BindRemove;
  bind t [[Control], (XKey "h")]  BindRemove;
  bind t [[Control], (XKey "d")]  BindRemove;
  bind t [[Control], (XKey "v")]  BindRemove;
  ()
;;

(* Set up a text widget to look like emacs *)

(* Navigation functions *)
let beginning_of_buffer t ev =
  text__mark_set t "insert" (TextIndex(TI_LineChar (0,0),[]))
and end_of_buffer t ev =
  text__mark_set t "insert" (TextIndex(TI_End, []))
and beginning_of_line t ev = 
  text__mark_set t "insert" (TextIndex(TI_Mark "insert", [LineStart]))
and end_of_line t ev = 
  text__mark_set t "insert" (TextIndex(TI_Mark "insert", [LineEnd]))

and backward_char t ev =
  text__mark_set t "insert" (TextIndex(TI_Mark "insert", [CharOffset (-1)]))
and forward_char t ev =
  text__mark_set t "insert" (TextIndex(TI_Mark "insert", [CharOffset 1]))

and previous_line t ev =
  text__mark_set t "insert" (TextIndex(TI_Mark "insert", [LineOffset (-1)]))
and next_line t ev =
  text__mark_set t "insert" (TextIndex(TI_Mark "insert", [LineOffset 1]))
;;

(* Edition functions *)
let backspace t ev =
  text__delete t 
      (TextIndex(TI_Mark "insert",[CharOffset (-1)])) 
      (TextIndex(TI_Mark "insert",[]))
;;

let delete_char t ev =
  text__delete t 
      (TextIndex(TI_Mark "insert",[]))
      (TextIndex(TI_Mark "insert",[CharOffset 1])) 
;;

let kill_to_eol t ev =
  text__delete t 
     (TextIndex(TI_Mark "insert",[])) 
     (TextIndex(TI_Mark "insert", [LineEnd]))
;;

let return_insert t ev =
  text__insert t (TextIndex(TI_Mark "insert",[])) "\n"
;;
let self_insert t ev =
  text__insert t (TextIndex(TI_Mark "insert",[])) ev.Ev_Char
;;

(* When no selection, produces an error ! *)
let safe_selection_get () =
  try selection__get ()
  with protocol__TkError _ -> ""
;;

let yank t ev =
  text__insert t (TextIndex(TI_Mark "insert",[])) (safe_selection_get ())
;;
let kill_region t ev =
  text__delete t (TextIndex (TI_TagFirst "sel", []))
      	       	 (TextIndex (TI_TagLast "sel", []))
;;

let wrap_redisplay f t =
  fun ev ->
    f t ev ;
    text__yview_pickplace t (TextIndex(TI_Mark "insert",[]))
;;

let wrap_noedit_redisplay f t = 
  fun ev ->
    let tags = 
      text__tag_indexnames t (TextIndex(TI_Mark "insert",[CharOffset (-1)])) in
      if not (mem "readonly" tags)
      then f t ev;
      text__yview_pickplace t (TextIndex(TI_Mark "insert",[]))
;;

let wrap_backspace_redisplay f t = 
  fun ev ->
    let tags = 
      text__tag_indexnames t (TextIndex(TI_Mark "insert",[CharOffset (-2)])) in
      if not (mem "readonly" tags)
      then f t ev;
      text__yview_pickplace t (TextIndex(TI_Mark "insert",[]))
;;

let wrap_kill_region_redisplay f t = 
  fun ev ->
    let tags = 
      text__tag_indexnames t (TextIndex(TI_TagFirst "sel", []))
    @ text__tag_indexnames t (TextIndex(TI_TagLast "sel", [])) in
      if not (mem "readonly" tags)
      then f t ev;
      text__yview_pickplace t (TextIndex(TI_Mark "insert",[]))
;;



let emacs_text t =
  text_clean_default t;
  (* Navigation commands *)
  bind t [[Meta], XKey "less"]     
      (BindSet([], (wrap_redisplay beginning_of_buffer t)));
  bind t [[Meta], XKey "greater"]  
      (BindSet([], (wrap_redisplay end_of_buffer t)));
  bind t [[Control], XKey "a"] 
      (BindSet([], (wrap_redisplay beginning_of_line t)));
  bind t [[Control], XKey "e"] 
      (BindSet([], (wrap_redisplay end_of_line t)));
  bind t [[Control], XKey "b"] 
      (BindSet([], (wrap_redisplay backward_char t)));
  bind t [[], XKey "Left"] 
      (BindSet([], (wrap_redisplay backward_char t)));
  bind t [[Control], XKey "f"] 
      (BindSet([], (wrap_redisplay forward_char t)));
  bind t [[], XKey "Right"] 
      (BindSet([], (wrap_redisplay forward_char t)));
  bind t [[Control], XKey "p"] 
      (BindSet([], (wrap_redisplay previous_line t)));
  bind t [[], XKey "Up"] 
      (BindSet([], (wrap_redisplay previous_line t)));
  bind t [[Control], XKey "n"] 
      (BindSet([], (wrap_redisplay next_line t)));
  bind t [[], XKey "Down"] 
      (BindSet([], (wrap_redisplay next_line t)));
  (* Edition commands *)
  bind t [[], XKey "Return"] 
      (BindSet([], (wrap_noedit_redisplay return_insert t)));
  bind t [[], XKey "Delete"] 
      (BindSet([], (wrap_backspace_redisplay backspace t)));
  bind t [[], XKey "BackSpace"] 
      (BindSet([], (wrap_backspace_redisplay backspace t)));
  bind t [[Control], XKey "h"] 
      (BindSet([], (wrap_backspace_redisplay backspace t)));
  bind t [[Control], XKey "d"] 
      (BindSet([], (wrap_noedit_redisplay delete_char t)));
  bind t [[Control], XKey "k"] 
      (BindSet([], (wrap_noedit_redisplay kill_to_eol t)));
  bind t [[Any], KeyPress] 
      (BindSet([Char], (wrap_noedit_redisplay self_insert t)));
  bind t [[Control], XKey "w"] 
      (BindSet([], (wrap_kill_region_redisplay kill_region t)));
  bind t [[Control], XKey "y"] 
      (BindSet([], (wrap_noedit_redisplay yank t)));
  ()
;;



(* goodies for index manipulation *)
let char_index tkindex offset =
  match tkindex with
    TI_LineChar(x,y) -> TextIndex(TI_LineChar(x,y),[CharOffset offset])
;;

let line_end tkindex = 
  match tkindex with
    TI_LineChar(x,y) -> TextIndex(TI_LineChar(x,y),[LineEnd])
;;
