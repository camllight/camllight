#open "comparisons";;
#open "type_for_matching";;


(************************** CONVERSION ***********************************)

let translate_frozen_variable
  {Max_depth = max;
   Depth = depth;
   Sub_nb = sub_nb;
   Fro_nb = fro_nb;
   Sub_tab = sub_tab;
   Fro_tab = fro_tab;
   Rest = rest}
  var
=
  let new_var = fro_nb + 1 in
    ({Max_depth = max;
      Depth = depth;
      Sub_nb = sub_nb;
      Fro_nb = new_var;
      Sub_tab = sub_tab;
      Fro_tab = (new_var, var)::fro_tab;
      Rest = rest},
     new_var);;


(************************** STATE ****************************************)

let frozen_var_bound {Fro_tab = tbl} (num, _) = mem_assoc num tbl;;

let frozen_var_value {Fro_tab = tbl} (num, _) =
  try
    assoc num tbl
  with
    Not_found ->
      failwith "There must be a bug somewhere.";;    

let subst_var_state {Sub_tab = tbl} (num, _) =
  snd (assoc num tbl);;


(************************** COMPARISONS **********************************)

(* Compare `var1' et `var2' *)
(* var1 var2 -> comp *)
let free_variable_compare (num1, _) (num2, _) =
  int_compare num1 num2;;

(* Is `variable1' less or egal to `variable2' *)
(* variable1 variable2 -> comp *)
let free_variable_less (a, _) (b, _) = a <= b;;

(* Compare `var1' et `var2' *)
(* var1 var2 -> comp *)
let frozen_var_compare env var1 (num2, _) =
  int_compare (frozen_var_value env var1) num2;;

(* Compare `var1' et `var2' *)
(* var1 var2 -> comp *)
let bound_var_compare env var1 var2 =
  int_compare (frozen_var_value env var1) (frozen_var_value env var2);;

(* Compare `const1' et `const2' *)
(* const1 const2 -> comp *)
let pattern_constant_compare (name1, _) (name2, _) =
  string_compare name1 name2;;

(* Is `const1' less or egal to `const2' *)
(* const1 const2 -> comp *)
let pattern_constant_less (name1, _) (name2, _) =
  le_string name1 name2;;

(* Compare `const1' et `const2' *)
(* const1 const2 -> comp *)
let rec subject_constant_compare ((name1, param1), _) ((name2, param2), _) =
  match string_compare name1 name2 with
    Equ -> list_compare subject_type_compare param1 param2
  | x -> x
and subject_type_compare
  {S_constant = c1; S_frozen_variable = v1}
  {S_constant = c2; S_frozen_variable = v2}
= 
  match list_compare subject_constant_compare c1 c2 with
    Equ -> list_compare free_variable_compare v1 v2
  | x -> x;;


(* Is `const1' less or egal to `const2' *)
(* const1 const2 -> comp *)
let subject_constant_less ((name1, param1), _) ((name2, param2), _) =
  (match string_compare name1 name2 with
     Equ -> list_compare subject_type_compare param1 param2
   | x -> x)
  != Sup;;

(************************** VARIABLES ASSOCIATION **********************************)

(* Associate `var1' and `var2' *)
(* var1 var2 continue *)
let frozen_var_associate
  {Max_depth = max;
   Depth = depth;
   Sub_nb = sub_nb;
   Fro_nb = fro_nb;
   Sub_tab = sub_tab;
   Fro_tab = fro_tab;
   Rest = rest}
  var1
  var2
=
  {Max_depth = max;
   Depth = depth;
   Sub_nb = sub_nb;
   Fro_nb = fro_nb;
   Sub_tab = sub_tab;
   Fro_tab = (var1, var2)::fro_tab;
   Rest = rest};;
(*
(* Associate `var1' and `var2' *)
(* var1 var2 continue *)
let subst_var_associate var1 var2 continue =
  vect_assign !subst_value var1 (Bound (ST var2));
  continue ();
  vect_assign !subst_value var1 Free;;
*)

(************************** VARIOUS FUNCTIONS ****************************)

(* Merge two ordered lists of functions *)
let function_merge compare =
  let rec merge =
    fun
      l1 [] -> l1
    | [] l2 -> l2
    | (((id1, parameter1) as func1)::tail1 as list1)
      (((_,   parameter2) as func2)::tail2 as list2) ->
      	match compare func1 func2 with
	  Inf ->
      	    func1::(merge tail1 list2)
	| Equ  ->
	    let
      	      {Count = n1; Parameter = arg1} = parameter1
	    and
      	      {Count = n2; Parameter = arg2} = parameter2
	    in
	      (id1, {Count = n1 + n2; Parameter = arg2@arg1})
      	       	::(merge tail1 tail2)
	| Sup ->
      	    func2::(merge list1 tail2)
  in merge;;

let new_subst_variable
  {Max_depth = max_depth;
   Depth = depth;
   Sub_nb = sub_nb;
   Fro_nb = fro_nb;
   Sub_tab = sub_tab;
   Fro_tab = fro_tab;
   Rest = rest}
  old_variable
=
  let
    new_depth =
      if old_variable = -1 then 0
      else (fst (assoc old_variable sub_tab)) + 1
  and
    new_var = sub_nb + 1
  in
    ({Max_depth = max_depth;
      Depth = max depth new_depth;
      Sub_nb = new_var;
      Fro_nb = fro_nb;
      Sub_tab = (new_var, (new_depth, Free))::sub_tab;
      Fro_tab = fro_tab;
      Rest = rest},
     new_var,
     new_depth);;

let rec new_subst_table =
  function
    -1 -> []
  | n -> (n, (0, Free))::(new_subst_table (n - 1));;

let merge_pattern_type
  {P_constant = constant1;
   P_frozen_variable = frozen1;
   P_subst_variable = subst1;
   P_arity = arity1}
  {P_constant = constant2;
   P_frozen_variable = frozen2;
   P_subst_variable = subst2;
   P_arity = arity2}
=
  {P_constant = function_merge pattern_constant_compare constant1 constant2;
   P_frozen_variable = function_merge free_variable_compare frozen1 frozen2;
   P_subst_variable = function_merge free_variable_compare subst1 subst2;
   P_arity = arity1 + arity2};;

let subst_variable_add
  {Max_depth = max_depth;
   Depth = depth;
   Sub_nb = sub_nb;
   Fro_nb = fro_nb;
   Sub_tab = sub_tab;
   Fro_tab = fro_tab;
   Rest = rest}
  variable
  type1
=
  let (var_depth, new_type) =
    match assoc variable sub_tab with
      (depth, Free) -> (depth, type1)
    | (depth, Bound type2) ->
      	(depth, merge_pattern_type type1 type2)
  in
    {Max_depth = max_depth;
     Depth = depth;
     Sub_nb = sub_nb;
     Fro_nb = fro_nb;
     Sub_tab = (variable, (var_depth, Bound new_type))::sub_tab;
     Fro_tab = fro_tab;
     Rest = rest};;

let change_rest
  {Max_depth = max;
   Depth = depth;
   Sub_nb = sub_nb;
   Fro_nb = fro_nb;
   Sub_tab = sub_tab;
   Fro_tab = fro_tab}
  rest
=
  {Max_depth = max;
   Depth = depth;
   Sub_nb = sub_nb;
   Fro_nb = fro_nb;
   Sub_tab = sub_tab;
   Fro_tab = fro_tab;
   Rest = rest};;
  
let push_type
  ({Rest = table} as env)
  ({P_subst_variable = lst; P_arity = a1} as type1)
  ({S_arity = a2} as type2)
  continue
=
  if lst = [] then
    (if a1 = a2 then
       if a1 = 0 then continue env (* Empty type *)
       else continue (change_rest env ((type1, type2)::table)))
  else
    if a1 <= a2 then
      continue (change_rest env ((type1, type2)::table));;

(************************** DEPTH ****************************************)

let partial_visit = ref false;;

let initialise () = 
  partial_visit := false;;
