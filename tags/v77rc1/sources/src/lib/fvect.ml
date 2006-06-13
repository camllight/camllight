(* Operations on vectors, without sanity checks *)

#open "eq";;
#open "int";;

let make_matrix dimx dimy init =
  let res = make_vect dimx [||] in
  for x = 0 to pred dimx do
    res.(x) <- make_vect dimy init
  done;
  res
;;  

let init_vect l f =
  if l = 0 then [||] else
   let res = make_vect l (f 0) in
   for i = 1 to pred l do
     res.(i) <- f i
   done;
   res;;

let blit_vect v1 start1 v2 start2 len =
  if start1 < start2 then
    (* Top-down copy *)
    for i = pred len downto 0 do
      v2.(start2 + i) <- v1.(start1 + i)
    done
  else
    (* Bottom-up copy *)
    for i = 0 to pred len do
      v2.(start2 + i) <- v1.(start1 + i)
    done
;;

let fill_vect v start len init =
  for i = start to pred(start + len) do v.(i) <- init done
;;

let copy_vect v =
  if vect_length v == 0 then v else begin
    let v' = make_vect (vect_length v) v.(0) in
    blit_vect v 1 v' 1 (vect_length v - 1);
    v'
  end
;;

let concat_vect v1 v2 =
  if vect_length v1 == 0 then copy_vect v2
  else if vect_length v2 == 0 then copy_vect v1
  else begin
    let v = make_vect (vect_length v1 + vect_length v2) v1.(0) in
      blit_vect v1 1 v 1 (vect_length v1 - 1);
      blit_vect v2 0 v (vect_length v1) (vect_length v2);
      v
    end
;;
let sub_vect v start len =
  if len == 0 then [| |] else begin
    let res = make_vect len v.(start) in
      for i = 1 to pred len do
        res.(i) <- v.(start+i)
      done;
      res
    end
;;
let list_of_vect v =
  let rec list_of i =
    if i >= vect_length v then [] else v.(i) :: list_of (succ i)
  in list_of 0
;;
let vect_of_list = function
    [] -> [| |]
  | a::rest as l ->
      let v = make_vect (list__list_length l) a in
      let rec fill_vect i = function
         []  -> ()
      | a::l -> v.(i) <- a; fill_vect (succ i) l
      in
        fill_vect 1 rest; v
;;
let do_vect f v =
  for i = 0 to pred(vect_length v) do f v.(i) done
;;
let map_vect f v =
  if vect_length v == 0 then [| |] else begin
    let res = make_vect (vect_length v) (f v.(0)) in
      for i = 1 to pred(vect_length v) do
        res.(i) <- f v.(i)
      done;
      res
    end
;;
let map_vect_list f v =
  let rec map i =
    if i >= vect_length v then [] else f v.(i) :: map (succ i)
  in
    map 0
;;

