#open "tk";;

let int_of_string s =
  try
    int_of_string s
  with
    _ -> 0
;;

let Bye () = CloseTk() ; exit 0
;;
let f = ref (prefix +)
;;

let Top = OpenTk ()
;;
let BAdd = button__create Top [Text "+" ; Relief Raised] ;;
let BSub = button__create Top [Text "-" ; Relief Raised] ;;
let BMult = button__create Top [Text "*" ; Relief Raised] ;;
let N1 = entry__create Top [Relief Sunken ; TextWidth 10] ;;
let N2 = entry__create Top [Relief Sunken ; TextWidth 10] ;;
let L1 = label__create Top [Text "+"] ;;
let L2 = label__create Top [Text "="] ;;
let L3 = label__create Top [Text "Undefined"] ;;
let BQuit = button__create Top [Text "Quit" ; Relief Raised ; Command Bye] ;;

let SetRes a b c d = 
  let tmp2 = int_of_string (entry__get N2) and tmp1 = int_of_string (entry__get N1) in
    label__configure L3 [Text (string_of_int (!f tmp1 tmp2))] ;;

let SetAdd = function () ->
  label__configure L1 [Text "+"] ;
  f := (prefix +) ;;

let SetSub = function () ->
  label__configure L1 [Text "-"] ;
  f := (prefix -) ;;

let SetMult = function () ->
  label__configure L1 [Text "*"] ;
  f := (prefix *) ;;

entry__configure N1 [ScrollCommand SetRes] ;;
entry__configure N2 [ScrollCommand SetRes] ;;
button__configure BAdd [Command SetAdd] ;;
button__configure BSub [Command SetSub] ;;
button__configure BMult [Command SetMult] ;;

pack [BAdd; BSub; BMult; N1; L1; N2; L2; L3; BQuit] [] ;;

printexc__f MainLoop () ;;
