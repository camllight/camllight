(* Profiler support *)
#open "ref";;
let counters = ref ([] : (string * (string * int vect)) list);;
