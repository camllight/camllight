#open "tk";;

let int_of_string s =
  try
    int_of_string s
  with
    _ -> 0
;;


let ActNumber1 = ref 0 ;;
let ActNumber2 = ref 0 ;;

let Top = OpenTk () ;;
let Number1 = entry__create Top [TextWidth 6 ; Relief (Sunken)] ;;
let Label1 = label__create Top [Text ("plus")] ;;
let Number2 = entry__create Top [TextWidth 6 ; Relief (Sunken)] ;;
let Label2 = label__create Top [Text ("=")] ;;
let Result = label__create Top [] ;;

let AdjustResultWithNumber1 = function a -> function b -> function c -> function d ->
  let EntryStr = entry__get Number1 in
    begin
    ActNumber1 := int_of_string EntryStr ;
    label__configure Result [Text (string_of_int (!ActNumber1 + !ActNumber2))]
    end ;;

let AdjustResultWithNumber2 = function a -> function b -> function c -> function d ->
  let EntryStr = entry__get Number2 in
    begin
    ActNumber2 := int_of_string EntryStr ;
    label__configure Result [Text (string_of_int (!ActNumber1 + !ActNumber2))]
    end ;;

entry__configure Number1 [ScrollCommand (AdjustResultWithNumber1)] ;;
entry__configure Number2 [ScrollCommand (AdjustResultWithNumber2)] ;;

pack [Number1; Label1; Number2; Label2; Result] [] ;;

let BindCmd = function (EvInfo) ->
    entry__configure Number1 [Background Blue] ;;

bind Number1 [[],XKey("A")] (BindSet([], BindCmd)) ;;
bind Number1 [[],XKey("B")] (BindSet([], BindCmd)) ;;
let f = frame__create Top [] ;;
pack [f] [] ;;
let r = dialog f ("-") ("Appuyez sur A ou B dans la zone de saisie du haut...") (Predefined "warning") (1) (["Yes sir"]) ;;

MainLoop () ;;
