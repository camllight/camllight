#open "tk";;

let OldX = ref 0 ;;
let OldY = ref 0 ;;

let Bye = function () -> CloseTk () ;  exit (0) ;;

let Top = OpenTk () ;;
let Quit = button__create Top [Text "Clean Exit"] ;;
let MyCan = canvas__create Top [Relief Groove ; Borderwidth (Millimeters 2.0)] ;;
let id = canvas__createline MyCan [Pixels 0; Pixels 0; Pixels 70; Pixels 70] 
                                  [ArrowStyle  Arrow_Both] ;;

canvas__addtag MyCan (Tag"daube") (Withtag id) ;;

let Kill = function () ->
  canvas__delete MyCan [(Tag "daube")] ; Bye () ;;
button__configure Quit [Command Kill] ;;

let BindCmd = function EvInfo ->
   canvas__move MyCan (Tag "daube") 
      	  (Pixels (EvInfo.Ev_MouseX- !OldX)) 
      	  (Pixels (EvInfo.Ev_MouseY- !OldY)) ;
   OldX := EvInfo.Ev_MouseX ;
   OldY := EvInfo.Ev_MouseY ;;

let Daube = function EvInfo ->
  print_string "\007" ; flush std_out ;;

bind MyCan [[],Motion] (BindSet([Ev_MouseX; Ev_MouseY;Ev_Place;Ev_SendEvent;Ev_KeySymString],BindCmd)) ;;
canvas_bind MyCan (Tag "daube")  [[],Motion] (BindSet([],Daube)) ;;

pack [Quit; MyCan] [] ;;

MainLoop () ;;
