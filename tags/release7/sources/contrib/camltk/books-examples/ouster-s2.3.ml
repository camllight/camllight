#open "tk";;

let top = OpenTk () in
let b = button__create top [Text "Hello, world!";
      	       	       	    Command CloseTk] in
  pack [b][];
  MainLoop()
;;

let top= OpenTk() in
let b = button__create top [Text "Hello, world!";
      	       	       	    Command (function () ->
      	       	       	       	       print_string "Good-bye!\n";
      	       	       	       	       flush stdout;
      	       	       	       	       CloseTk())] in
  pack [b][];
  MainLoop()
;;
