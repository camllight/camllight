(* $Id$ *)

#open "asl";;
#open "main";;

input_stream := stdin;;

if sys__interactive then () else begin go(); exit 0 end;;
