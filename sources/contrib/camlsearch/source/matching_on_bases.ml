#open "type_for_matching";;
#open "operations_on_types";;

(****** matching on bases ********)

let rec bound_with_unit parb=fun
    [] -> parb
  | ((x,xparam)::xrest) -> subst_variable_add (bound_with_unit parb xrest) x Unit;;

(* y (as int) => y (as PATTERN_TYPE) *)
let pattern_subst_var y={P_constant=[];P_frozen_variable=[];
      	       	       	 P_subst_variable=[y,{Count=1;Parameter=[Unit]}];P_arity=0};;

(* (A1,...,An cst => (_x1,...,_xn) cst *)
let rec create_const_param parb=fun
    [] -> (parb,[])
  | (a::l) -> (let (parb2,y,depth)=new_subst_variable parb (-1)
               in
	         (let (parb3,yl)=create_const_param parb2 l
                  in
		    (parb3,((pattern_subst_var y)::yl))));;

(* newpatt => newpatt*(A->cst) *)
let add_c=fun
    (cst,ccount,param) ({P_constant=(((pcst,{Count=count;
      	       	        	     Parameter=parl})::rest) as rest2)} as foo)
       -> if pcst=cst then     
               {P_constant=((cst,{Count=count+ccount;
	                           Parameter=(param@parl)})::rest);
                P_frozen_variable=foo.P_frozen_variable;
		P_subst_variable=[];
                P_arity=foo.P_arity+ccount}
      	  else
      	       {P_constant=((cst,{Count=ccount;
	                          Parameter=param})::rest2);
                P_frozen_variable=foo.P_frozen_variable;
		P_subst_variable=[];
                P_arity=foo.P_arity+ccount}
   | (cst,ccount,param) ({P_constant=[]} as foo)
        ->  {P_constant=[cst,{Count=ccount;
	                          Parameter=param}];
             P_frozen_variable=foo.P_frozen_variable;
             P_subst_variable=[];
             P_arity=foo.P_arity+ccount};;

(* newpatt => newpatt*(A->fro) *)
let add_f=fun
    (cst,ccount,param) ({P_frozen_variable=(((pcst,{Count=count;
      	       	        	     Parameter=parl})::rest) as rest2)} as foo)
    ->  if pcst=cst then
               {P_frozen_variable=((cst,{Count=count+ccount;
	                           Parameter=(param@parl)})::rest);
                P_constant= foo.P_constant;
		P_subst_variable=[];
                P_arity=foo.P_arity+ccount}
	else
      	       {P_frozen_variable=((cst,{Count=ccount;
	                          Parameter=param})::rest2);
                P_constant=foo.P_constant;
		P_subst_variable=[];
                P_arity=foo.P_arity+ccount}
  | (cst,ccount,param) ({P_frozen_variable=[]} as foo)
    ->         {P_frozen_variable=[cst,{Count=ccount;
	                          Parameter=param}];
                P_constant=foo.P_constant;
		P_subst_variable=[];
                P_arity=foo.P_arity+ccount};;

(* A => A*y *)
let add_param y cstparam=fun
    {P_constant=pc;
     P_frozen_variable=pf;
     P_subst_variable=ps;
     P_arity=pa} -> {P_constant=pc;P_frozen_variable=pf;P_subst_variable=ps@[y,{Count=1;               
                                                                                Parameter=[Unit]}];P_arity=pa}::cstparam;;


(* A => A*y *)
let add_param_f y=fun
    {P_constant=pc;
     P_frozen_variable=pf;
     P_subst_variable=ps;
     P_arity=pa} -> {P_constant=pc;P_frozen_variable=pf;P_subst_variable=ps@[y,
                               {Count=1 ; Parameter=[Unit]}];P_arity=pa};;

let is_bound_with_unit subst_list x = 
    match assoc x subst_list with
       (depth, Bound val) -> (depth = 0) && (val = Unit)
     | _ -> false;;


let rec change_max_depth parb=fun
    [] -> parb
  | ((x,xparam)::xrest) -> if is_bound_with_unit parb.Sub_tab x then
      	       	       	   change_max_depth
      	       	       	   { Max_depth=parb.Max_depth-Unit_cost;
                             Depth=parb.Depth;
                             Sub_nb=parb.Sub_nb;
			     Fro_nb=parb.Fro_nb;
			     Sub_tab=parb.Sub_tab;
			     Fro_tab=parb.Fro_tab;
			     Rest=parb.Rest } xrest
                           else
			   change_max_depth parb xrest;;

let check_depth parb xlist=let parbb=change_max_depth parb xlist
                           in
			   (parbb,(parbb.Depth<parbb.Max_depth));;

(********************************************)
let rec match_frozen continue parb newpatt px pc pf f=fun
    xlist 0 fro -> match_f_base continue parb newpatt px pc pf f
  | [] _ _      -> () (* failed : too many frozen variable *)
  | (((x,xparam)::xrest) as xlist) count fro ->
                if count>=xparam.Count then
                (let (parb2,y,depth)=(new_subst_variable parb x)
		  in
                  if depth>=parb2.Max_depth then
		    partial_visit:=true
                  else
		   (let (parb3,fro2)=translate_frozen_variable parb2 fro
                    in
		     (let parb4=subst_variable_add parb3 x 
                                              {P_constant=[];
      	       	       	       	       	       P_frozen_variable=[fro2,{Count=1;
                                                         Parameter=[pattern_subst_var y]}];
					       P_subst_variable=[];
					       P_arity=1} (* X = X * (Y -> fro2) *)
                      in
      	       	        (match_frozen continue parb4
                                  (add_f (fro2,xparam.Count,
                                  (map (add_param_f y) xparam.Parameter)) newpatt)
                                   px pc pf f xlist (count-xparam.Count) fro))));
                match_frozen continue parb newpatt px pc pf f xrest count fro
and
 match_f_base continue parb newpatt px pc pf=fun
    [] -> let (parbc,ok)=(check_depth parb px)
          in
          (if ok then
           (push_type parb 
           (* newpattern *)
           {P_constant=(rev newpatt.P_constant);P_frozen_variable=(rev newpatt.P_frozen_variable);
            P_subst_variable=[];P_arity=newpatt.P_arity}
           (* newsubject *)
           {S_constant=pc;S_frozen_variable=pf;S_arity=newpatt.P_arity} continue)
           else
            partial_visit:=true)
  | ((fro,fparam)::f)-> 
 match_frozen continue parb newpatt px pc pf f px 
                                         fparam.Count fro;;

(********************************************)
let rec match_const continue parb newpatt px pc pf c=fun
    xlist 0 cst -> match_c_base continue parb newpatt px pc pf c
  | [] _ _      -> () (* failed : too many constant *)
  | (((x,xparam)::xrest) as xlist) count cst ->
                if count>=xparam.Count then
               (* matches _X^p on cst^q where q>=p *)
		 (let (parb2,y,depth)=(new_subst_variable parb x)
		  in
		  if depth>=parb2.Max_depth then
                     partial_visit:=true
                  else
		 (let parb3=subst_variable_add parb2 x 
                                              {P_constant=[(fst cst),{Count=1;
                                                         Parameter=[(pattern_subst_var y)::(snd cst)]}];
		                                  P_frozen_variable=[];
						  P_subst_variable=[];
						  P_arity=1} (* _X <= _X * (_Y -> (_x1,...,_xn) cst) *)
		  in
      	       	    match_const continue parb3
                               (add_c ((fst cst),xparam.Count,
                                (map (add_param y (snd cst)) xparam.Parameter)) newpatt)
				(* (A1->X)*...*(An->X) => (A1*_y->cst)*...*(An*_y->cst) *)
                                px pc pf c xlist (count-xparam.Count) cst));
	       (* if failed *)
	        match_const continue parb newpatt px pc pf c xrest count cst
and
(* matches on constant base *)
 match_c_base continue parb newpatt px pc pf=fun
   (* no more const => matches on frozen base *)
    [] -> match_f_base continue parb newpatt px pc pf pf
  | ((cst,cparam)::c)-> let (parb2,cstparam)=(create_const_param parb (snd cst))
                        (* (A1,...,An) cst => (_x1,...,_xn) cst *)
                        in
                          match_const continue parb2 newpatt px pc pf c px 
                                         cparam.Count ((fst cst),cstparam);;


(*************************** main function ******************************)
let match_base continue parb=fun
   (* no variable, no frozen, no const ! *)
    [] [] [] -> continue parb (* to be continued ... *)
  | [] _  _  -> () (* failed : no variable to match the base *)
  | px pc pf -> let parbb=bound_with_unit parb px
                in
                 match_c_base continue parbb Unit px pc pf pc;;
