#open "tk";;

(* Linking a text widget with a scrollbar widget *)

let main () =
  let top = OpenTk() in
  let tw = text__create top [TextWidth 20] 
  and sb = scrollbar__create top [] 
  and q = button__create top 
      	    [Text "Quit"; Command (function () -> CloseTk(); exit 0)] in

    scrollbar__configure sb [Slidecommand (text__yview_line tw)];
    text__configure tw [YScrollCommand (scrollbar__set sb)];

    pack [sb] [Fill Fill_Y;Side Side_Right];
    pack [tw; q] [];
    MainLoop()
;;


printexc__f main ()
;;
