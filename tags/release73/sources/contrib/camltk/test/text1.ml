#open "tk";;

let main () =
  let top = OpenTk() in
  let t = text__create top [TextWidth 20; TextHeight 10; Wrap WrapWord] in
  let print_contents () =
    print_string (text__get t (TextIndex(TI_LineChar(1,0),[]))
      	       	       	      (TextIndex(TI_End,[])));
    flush std_out in
  let b = button__create top [Text "Get"; Command print_contents] 
  and q = button__create top [Text "Quit"; 
      	       	       	      Command (function () -> CloseTk(); exit 0)] in
  
    pack [q;b;t] [];
    MainLoop()
;;


printexc__f main ()
;;

