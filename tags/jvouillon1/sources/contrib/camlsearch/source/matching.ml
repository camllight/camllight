#open "general";;
#open "primitives";;
#open "comparisons";;
#open "type_for_matching";;
#open "operations_on_types";;
#open "matching_on_bases";;

let match_type_list env l1 l2 continue =
  let rec traverse env2 =
    fun
      [] [] -> continue env2
    | (a::l) (b::m) ->
        push_type env2 a b (function env3 -> traverse env3 l m)
    | _ _ -> raise Wrong_number_of_parameters
  in
    traverse env l1 l2;;

(* Match `arg1' and `arg2' *)
(* continue env arg1 arg2 *)
let single_argument_match env arg1 arg2 continue =
  let rec traverse1 =
    fun
      env [] rest2 ->
      	continue env rest2
    | _ _ [] ->
        ()
    | env (elem1::arg1) rest2 ->
      	apply_element
      	  (fun elem2 rest ->
	     push_type
               env
               elem1
               elem2
               (function env -> traverse1 env arg1 rest))
      	  rest2
  in traverse1 env arg1 arg2;;

(* Match some elements of `arg1' on some elements of `arg2' *)
(* continue env arg1 arg2 *)
let multiple_argument_match
  env
  arg1
  (((base2, parameter2), {Parameter = arg2}) as funct2)
  continue
=
  let rec traverse modif env beginning2 =
    fun
      [] rest2 ->  
      	continue
	  env
	  []
	  (if modif then
	     let new_param = unite_rev beginning2 rest2 in
	       if new_param = [] then
	         []
	       else
                 [((base2, parameter2),
      	       	   {Count = list_length new_param;
      	       	    Parameter = new_param})]
	   else
	     [funct2])
    | rest1 [] ->
      	continue
	  env
	  rest1
	  (if modif then
	     if beginning2 = [] then
	       []
	     else
               [((base2, parameter2),
      	       	 {Count = list_length beginning2; Parameter = rev beginning2})]
	   else
	     [funct2])
    | rest1 (elem2::rest2) ->
    	apply_element
      	  (fun elem1 rest1b ->
	     match_type_list
               env
               elem1
              (elem2::parameter2)
              (function env -> traverse true env beginning2 rest1b rest2))
      	  rest1;
	traverse modif env (elem2::beginning2) rest1 rest2
  in traverse false env [] arg1 arg2;;

let constant_match env l1 l2 continue =
  let rec loop env2 beginning2a end2a =
    function
      (((base1, {Count = n1; Parameter = arg}) as elem1)::l1) ->
        let rec find_correspondent env3 arg1 beginning2 =
          function
            ((((base2, _), _) as elem2)::end2)
                as next2 ->
              (match string_compare base1 base2 with
                 Equ  ->
                   multiple_argument_match
                     env3
                     arg1
		     elem2
                     (fun env4 rest1 rest2 ->
                        find_correspondent
                          env4
                          rest1
		          (rest2 @ beginning2)
                          end2)
               | Sup ->
                   find_correspondent
                     env3
                     arg1
                     (elem2::beginning2)
                     end2
               | _ (* Inf *)  ->
                   match arg1 with
                     [] -> 
                       loop
                         env3
                         beginning2
                         next2
			 l1
                   | _ -> ())
          | _ ->
              match arg1 with
                [] -> 
                  loop
                    env3
                    beginning2
                    []
	            l1
              | _ -> ()
        in
          find_correspondent env2 arg beginning2a end2a
    | []  ->
        continue env2 (unite_rev beginning2a end2a)
  in loop env [] l2 l1;;

(* Matching of functions whose base is a frozen variable *)
(* continue env list1 list2 *)
let frozen_match env l1 l2 continue =
  let rec loop env2 =
    function
      (((_, parameter1) as elem1)::l1) ->
        let rec find_correspondent beginning2 =
          function
            (((base2, parameter2) as elem2)::end2) ->
              (match frozen_var_compare env elem1 elem2 with
      	         Sup ->
      	       	   find_correspondent (elem2::beginning2) end2
               | Equ  ->
	           let
      	       	     {Count = n1; Parameter = arg1} = parameter1
	           and
      	       	     {Count = n2; Parameter = arg2} = parameter2
	           in
      	             if n1 <= n2 then
		       single_argument_match 
			 env2
			 arg1
      	       	         arg2
                         (fun env3 rest2 ->
                            loop
			      env3
                              l1
                              (if rest2 = [] then
                                 beginning2
                               else
                                 (base2,
      	       	       	       	  {Count = (n2 - n1); Parameter = rest2})
                                ::beginning2)
                              end2)
               | _   -> ())
          | _ -> ()
        in find_correspondent
    | [] ->
      	fun debut2 fin2 ->
          continue env2 (unite_rev debut2 fin2)
  in loop env l1 [] l2;;

let renormalize_constant
  lst_arg2
  (name, {Count = n; Parameter = parameter})
=
  (name,
   {Count = n * (list_length lst_arg2);
    Parameter =
      flat_map
        (function
           arg2 ->
	     map
	       (function arg1::param ->
	          (merge_pattern_type arg1 arg2)::param
      	       	| _ -> failwith "???renormalize_constant")
      	       parameter)
      lst_arg2});;
  
let renormalize_frozen
  lst_arg2
  (name, {Count = n; Parameter = lst_arg1})
=
  (name,
   {Count = n * (list_length lst_arg2);
    Parameter =
      flat_map
        (function
           arg2 ->
	     map
	       (function arg1 -> merge_pattern_type arg1 arg2)
      	       lst_arg1)
      lst_arg2});;
  
(* Expanse bound substitution variables *)
let expanse_subst_variable env typ =
  let
    {P_constant = constant;
     P_frozen_variable = frozen;
     P_subst_variable = subst;
     P_arity = arity1}
  =
    typ
  in
    list_it
      (fun
         subst_variable2
         (constant1, frozen1, subst1)
       ->
         match (subst_var_state env subst_variable2) with
           Bound {P_constant = constant2; P_frozen_variable = frozen2} ->
	     (match subst_variable2 with
	        (_, {Parameter = types}) ->
  	          (function_merge
                     pattern_constant_compare
                     constant1
                     (map (renormalize_constant types) constant2),
  	           function_merge
                     free_variable_compare
                     frozen1
                     (map (renormalize_frozen types) frozen2),
                   subst1))
         | Free ->
             (constant1, frozen1, (subst_variable2::subst1)))
      subst
      (constant, frozen, []);;

(* Remove repetitions in a list of bound frozen variables *)
let bound_var_compress env =
  let rec traverse =
    function
      [] -> []
    | [a] -> [a]
    | (a::((b::l) as l1)) ->
        if (bound_var_compare env a b = Equ) then
      	  let (var, {Count = n1; Parameter = param1}) = a
	  and (_,   {Count = n2; Parameter = param2}) = b
	  in
      	    traverse
      	      ((var, {Count = n1 + n2; Parameter = param1 @ param2})::l)
        else
      	  a::(traverse l1)
  in traverse;;

let free_frozen_var_match0
  continue 
  (nm, {Count = n; Parameter = param})
  env
=
  let rec traverse beginning =
    function
      [] -> ()
    | (((nm2, {Count = n1; Parameter = param0}) as elem)::tail) ->
      	if n <= n1 then
	  let env2 = frozen_var_associate env nm nm2 in
  	    single_argument_match
  	      env2
  	      param
              param0
  	      (fun env3 rest ->
                 continue
		   env3
        	   (unite_rev
        	      beginning
  		      (if n < n1 then
        	      	 ((nm2, {Count = n1 - n; Parameter = rest})::tail)
  		       else
  		         tail)));
  	traverse (elem::beginning) tail
  in traverse [];;

(* Matching of free frozen variables *)
let free_frozen_var_match env l1 l2 continue =
  let rec loop =
    fun
      []      -> continue
    | (a::l1) -> free_frozen_var_match0 (loop l1) a
  in loop l1 env l2;;

(* Match the bases of two types *)
let rec type_match0 continue
      	       	    env
      	            type1
      	            {S_constant = constant2; 
      	             S_frozen_variable = frozen2}
=
  let (constant1, frozen1, subst1) = expanse_subst_variable env type1 in
    let (bound, free) = list_split (frozen_var_bound env) frozen1 in
      let bound2 =
      	bound_var_compress
	  env
      	  (sort__sort (fun x y -> ((bound_var_compare env) x y != Sup)) bound)
      in
      	constant_match
	  env
	  constant1
	  constant2
	  (fun env rest_constant ->
      	     frozen_match
	       env
	       bound2
	       frozen2
	       (fun env rest_frozen ->
	          free_frozen_var_match
		    env
		    free
		    rest_frozen
		    (fun table rest_frozen ->
		  (* Variables de substitution *)
		       match_base
      	       	       	 continue table subst1 rest_constant rest_frozen)));;

(* Match two types *)
let type_match
  depth
  continue
  ((froz, subst, ty1) as type1)
  ((_, ty2) as type2)
= 
  let rec reduce =
    function
      {Rest = []} -> continue ()
    | {Rest = (t1, t2)::l} as tbl ->
        type_match0 reduce (change_rest tbl l) t1 t2
  in
    try
      initialise ();
      push_type
        {Max_depth = depth;
         Depth = 0;
         Sub_nb = subst;
         Fro_nb = froz;
         Sub_tab = new_subst_table subst;
         Fro_tab = [];
         Rest = []}
        ty1
        ty2
        reduce;
      !partial_visit
    with
      Wrong_number_of_parameters -> false;;
