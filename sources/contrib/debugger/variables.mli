(****************************** Variables **********************************)

#open "const";;
#open "primitives";;
#open "lambda";;
#open "globals";;
#open "value";;

(*** Printing a variable name. ***)
value output_variable_name : out_channel -> global_reference -> unit;;

(*** Value and type of a variable. ***)
value variable : global_reference -> VALUE * typ;;

