(* "Linear feedback shift register" random number generator. *)
(* References: Robert Sedgewick, "Algorithms", Addison-Wesley *)

#open "float";;
#open "int";;
#open "eq";;
#open "ref";;
#open "fvect";;

(* This is the state you get with [init 27182818]. *)
let state = [|
  27182818; 49616251; -713408192; -773140415; 184774990;
  -742824713; 1034277516; -122742499; -379589510; 184143411;
  433271448; 112550329; -901847962; 938020143; 345894244;
  927712789; -1025522414; -865624597; -403419408; 796140081;
  119716478; 251640935; 786654012; 222302733; -113987414;
  245458083; -836569528; 518844841; 1039881110; -736210273;
  -10382316; -574628603; -1035923646; -848952741; -883914592;
  144814113; -593424466; 780251095; -26222612; -92600579;
  -130858790; -1001367789; -459643400; 407911833; -201830714;
  -184372209; -682514748;-216174603; 587572594; -363380021;
  -355865008; -109938159; 426864862; 563494727; -977909604
|];;

let j = ref 0;;

let init seed =
  j := 0;
  state.(0) <- seed;
  for i = 1 to 54
  do state.(i) <- state.(i-1) * 31415821 + 1
  done
;;

let raw () =
  j := (!j + 1) mod 55;
  state.(!j) <- state.((!j+24) mod 55) lxor state.(!j);
  state.(!j) land 0x3FFFFFFF
;;

let int bound = raw () mod bound;;

let float bound = float_of_int (raw ()) /. 1073741824.0 *. bound;;
