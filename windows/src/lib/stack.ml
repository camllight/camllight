#open "list";;
#open "exc";;
#open "eq";;
#open "int";;

type 'a t = { mutable c : 'a list };;

let new () = { c = [] };;

let clear s = s.c <- [];;

let push x s = s.c <- x :: s.c;;

let rec item n = function
    [] -> raise Empty
  | x::l -> if n = 0 then x else item (n-1) l;;

let peek s n = item n s.c;;

let pop s =
  match s.c with
    hd::tl -> s.c <- tl; hd
  | []     -> raise Empty
;;

let length s = list_length s.c;;

let iter f s = do_list f s.c;;
