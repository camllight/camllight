(* Hash tables *)

#open "exc";;
#open "int";;
#open "eq";;
#open "fvect";;

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type ('a, 'b) t =
  { mutable max_len: int;                    (* max length of a bucket *)
    mutable data: ('a, 'b) bucketlist vect } (* the buckets *)

and ('a, 'b) bucketlist =
    Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist
;;

let new initial_size =
 if initial_size <= 0 then invalid_arg "hashtbl__new" else
 { max_len = 3; data = make_vect initial_size Empty }
;;

let clear h =
  for i = 0 to vect_length h.data - 1 do
    h.data.(i) <- Empty
  done
;;

let resize tbl =
  let odata = tbl.data in
  let osize = vect_length odata in
  let nsize = 2 * osize + 1 in
  let ndata = make_vect nsize Empty in
  let rec insert_bucket idx = function
      Empty -> ()
    | Cons(key, data, rest) ->
        insert_bucket idx rest; (* preserve original order of elements *)
        let nidx = hash_param 10 100 key mod nsize in
        ndata.(nidx) <- Cons(key, data, ndata.(nidx)) in
  for i = 0 to osize - 1 do
    insert_bucket i odata.(i)
  done;
  tbl.data <- ndata;
  tbl.max_len <- 2 * tbl.max_len
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
          f k d; do_bucket rest in
    do_bucket h.data.(i)
  done
;;

let do_table_rev f h =
  let rec do_bucket = function
      Empty ->
        ()
    | Cons(k, d, rest) ->
        do_bucket rest; f k d; () in
  for i = 0 to vect_length h.data - 1 do
    do_bucket h.data.(i)
  done
;;

let hash x = hash_param 50 500 x
;;
