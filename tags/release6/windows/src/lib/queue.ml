#open "int";;
#open "ref";;
#open "exc";;
#open "queue";;

type 'a queue_cell =
    Nil
  | Cons of 'a * 'a queue_cell ref
;;

type 'a t =
  { mutable head: 'a queue_cell;
    mutable tail: 'a queue_cell }
;;

let new () =
  { head = Nil; tail = Nil }
;;

let clear q =
  q.head <- Nil; q.tail <- Nil
;;

let add x = function
    { head = Nil as h; tail = Nil as t } ->
      let c = Cons(x, ref Nil) in
        h <- c; t <- c
  | { tail = Cons(_, ref newtail) as oldtail } ->
      let c = Cons(x, ref Nil) in
        newtail <- c; oldtail <- c
;;

let peek q =
  match q.head with
    Nil ->
      raise Empty
  | Cons(x, ref rest) ->
      x
;;


let take q =
  match q.head with
    Nil ->
      raise Empty
  | Cons(x, ref rest) ->
      q.head <- rest;
      begin match rest with
        Nil -> q.tail <- Nil
      |  _  -> ()
      end;
      x
;;

let rec length_aux = function
    Nil -> 0
  | Cons(_, ref rest) -> succ (length_aux rest)
;;

let length q = length_aux q.head
;;

let rec iter_aux f = function
    Nil ->
      ()
  | Cons(x, ref rest) ->
      f x; iter_aux f rest
;;

let iter f q = iter_aux f q.head
;;
