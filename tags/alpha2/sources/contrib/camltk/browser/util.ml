(* Utils *)
#open "tk";;

(* Make a window (toplevel widget) resizeable *)
let resizeable t =
  update(); (* wait until layout is computed *)
  wm__minsize_set t (winfo__width t) (winfo__height t)
;;

(* Link a scrollbar and a text widget *)
let scroll_text_link sb tx =
  text__configure tx [YScrollCommand (scrollbar__set sb)];
  scrollbar__configure sb [Slidecommand (text__yview_line tx)]
;;


(* Link a scrollbar and a listbox *)
let scroll_listbox_link sb lb =
  listbox__configure lb 
      	[YScrollCommand (scrollbar__set sb)];
  scrollbar__configure sb 
        [Slidecommand (fun n -> listbox__yview lb (Number n))]
;;
