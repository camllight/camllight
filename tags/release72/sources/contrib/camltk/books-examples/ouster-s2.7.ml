#open "tk";;
#open "printf";;

let top = OpenTk() in

bind top [[Any], Motion]
  (BindSet ([MouseX; MouseY],
      	(function ev ->
	  printf "pointer at %d, %d\n" ev.Ev_MouseX ev.Ev_MouseY;
      	  flush stdout)));

MainLoop()
;;

