(****************************** Variables **********************************)

#open "globals";;
#open "value";;

(*** Converting a variable name to a string. ***)
value string_of_variable_name : global_reference -> string;;

(*** Printing a variable name. ***)
value output_variable_name : out_channel -> global_reference -> unit;;

(*** Value and type of a variable. ***)
value variable : global_reference -> VALUE * typ;;

