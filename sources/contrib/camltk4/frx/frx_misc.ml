(* Frx was written for CSL. These are compatibility definitions *)

type 'a option = None | Some of 'a
;;

let float = float_of_int
;;
let truncate = int_of_float
;;

let rec nth l n =
  match l with
    [] -> failwith "nth"
  | a::l -> if n <= 0 then a else nth l (n-1)
;;


let autodef f =
  let v = ref None in
  (function () ->
     match !v with
       None ->
       	 let x = f() in
       	   v := Some x;
	   x
     | Some x -> x)
;;
