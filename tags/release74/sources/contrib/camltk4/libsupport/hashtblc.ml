(* Hash tables, without space leaks *)

#open "exc";;
#open "int";;
#open "eq";;
#open "fvect";;

(* We do dynamic hashing, and we double the size of the table when
   buckets become too long, re-hashing the elements. *)

type ('a, 'b) t =
  { mutable max_len: int;                    (* max length of a bucket *)
    mutable data: ('a, 'b) bucketlist vect } (* the buckets *)

and ('a, 'b) bucketlist =
    Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist
;;

let new initial_size =
  { max_len = 2; data = make_vect initial_size Empty }
;;

let clear h =
  for i = 0 to vect_length h.data - 1 do
    h.data.(i) <- Empty
  done
;;

(* Occasionnally, a bucket may become too long during a resize *)
let resize h =
  let n = vect_length h.data
  and olddata = h.data in
  let deuxn = n+n+1 in
  let newdata = make_vect deuxn Empty in
    h.data <- newdata;
    h.max_len <- 2 * h.max_len;
    for i = 0 to n - 1 do
    let rec do_bucket = function
        Empty -> ()
      | Cons(k, d, rest) ->
      	  let j = (hash_param 10 100 k) mod deuxn in
	  let bucket = Cons(k,d,newdata.(j)) in
	  newdata.(j) <- bucket;
	  do_bucket rest in
    do_bucket olddata.(i)
    done
;;

let rec bucket_too_long n bucket =
  if n < 0 then true else
    match bucket with
      Empty -> false
    | Cons(_,_,rest) -> bucket_too_long (pred n) rest
;;

let add h key info =
  let i = (hash_param 10 100 key) mod (vect_length h.data) in
  let bucket = Cons(key, info, h.data.(i)) in
    h.data.(i) <- bucket;
    if bucket_too_long h.max_len bucket then resize h
;;

let remove h key =
  let rec remove_bucket = function
      Empty ->
        Empty
    | Cons(k, i, next) ->
        if k = key then next else Cons(k, i, remove_bucket next) in
  let i = (hash_param 10 100 key) mod (vect_length h.data) in
    h.data.(i) <- remove_bucket h.data.(i)
;;

let find h key =
  match h.data.((hash_param 10 100 key) mod (vect_length h.data)) with
    Empty -> raise Not_found
  | Cons(k1, d1, rest1) ->
      if key = k1 then d1 else
      match rest1 with
        Empty -> raise Not_found
      | Cons(k2, d2, rest2) ->
          if key = k2 then d2 else
          match rest2 with
            Empty -> raise Not_found
          | Cons(k3, d3, rest3) ->
              if key = k3 then d3 else begin
                let rec find = function
                    Empty ->
                      raise Not_found
                  | Cons(k, d, rest) ->
                      if key = k then d else find rest
                in find rest3
              end
;;

let find_all h key =
  let rec find_in_bucket = function
    Empty ->
      []
  | Cons(k, d, rest) ->
      if k = key then d :: find_in_bucket rest else find_in_bucket rest in
  find_in_bucket h.data.((hash_param 10 100 key) mod (vect_length h.data))
;;

let do_table f h =
  let len = vect_length h.data in
  for i = 0 to vect_length h.data - 1 do
    let rec do_bucket = function
        Empty ->
          ()
      | Cons(k, d, rest) -> 
          let _ = f k d in do_bucket rest in
    do_bucket h.data.(i)
  done
;;

let do_table_rev f h =
  let len = vect_length h.data in
  for i = 0 to vect_length h.data - 1 do
    let rec do_bucket = function
        Empty ->
          ()
      | Cons(k, d, rest) ->
          do_bucket rest; let _ = f k d in () in
    do_bucket h.data.(i)
  done
;;

let hash x = hash_param 50 500 x
;;
