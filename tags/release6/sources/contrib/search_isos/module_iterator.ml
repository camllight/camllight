#open "const";;
#open "globals";;
#open "hashtbl";;
#open "modules";;

(* get the module interface *)
let interface_of module_name = 
        let ic = open_in_bin module_name in
        let m = (input_value ic : module) in
        close_in ic;
        m;;

(* get the list of values out of the hash_table *)
let values_of ({mod_values=values} : module) =
        let vlist = ref []
        in  let collect key data = vlist := data::!vlist
            in do_table collect values;
        !vlist;;

(* get the real list of value-type pair out of the value_desc global list of values *)
(* Design Decision: might iterate directly on the hash table ... what is more portable,
                    safe, robust? *)

(* PROBLEM: do_table may produce duplicates! do we sort them out? *)

let tvpairs_of_values vlist =
        let collect_tv {qualid = theId; info = {val_typ=theType}} = (theId, theType)
        in map collect_tv vlist;;

let tvpairs_of module_name = 
        tvpairs_of_values (values_of (interface_of module_name));;


(* This is the real iterator  function: takes a  list of proper module names
   (i.e. with the .zi extension present) and a  function that can operate on
   a type-value pair, and  iterates it on the  iterfaces of the  module name
   list, returning the list of the results *)

let rec iterate_on_module_interface f =
  function
    [] -> []
  | m::r -> (map f (tvpairs_of m))@(iterate_on_module_interface f r);;

(* for example:

let just_the_name ({qual=q;id=i},_) = print_string (q^"_"^i); print_newline();(q,i);;

allows to print the identifier names in the module interface *)
