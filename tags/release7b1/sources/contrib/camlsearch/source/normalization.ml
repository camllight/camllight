#open "external_type";;
#open "type_for_matching";;
#open "operations_on_types";;

type INTERMEDIARY_TYPE =
  {Cst : ((string * INTERMEDIARY_TYPE list) * INTERMEDIARY_TYPE) list;
   Froz : (int * INTERMEDIARY_TYPE) list;
   Subst : (int * INTERMEDIARY_TYPE) list;
   Ar_c : int;
   Ar_f : int};;

let full_iso = ref false;;

let empty_type = {Cst = []; Froz = []; Subst = []; Ar_c = 0; Ar_f = 0};;
let frozen_empty_type = {Cst = [(("$$$", []), empty_type)]; Froz = []; Subst = []; Ar_c = 1; Ar_f = 0};;

let rec passe_1 =
  function
    ET_frozen_variable n -> 
      {Cst = []; Froz = [(n, empty_type)]; Subst = []; Ar_c = 0; Ar_f = 1}
  | ET_subst_variable  n ->
      {Cst = []; Froz = []; Subst = [(n, empty_type)]; Ar_c = 0; Ar_f = 0}
  | ET_function (t2, t1) ->
      let {Cst = c2; Froz = f2; Subst = s2; Ar_c = ac2; Ar_f = af2} =
        passe_1 t2
      and {Cst = c0; Froz = f0; Subst = s0; Ar_c = ac0; Ar_f = af0} =
        passe_1 t1
      in
        let explode =
      	  function
	    (t, {Cst = c1; Froz = f1; Subst = s1; Ar_c = ac1; Ar_f = af1}) ->
	      (t, {Cst = c1 @ c2;
                   Froz = f1 @ f2;
                   Subst = s1 @ s2;
                   Ar_c = ac1 + ac2;
      	       	   Ar_f = af1 + af2})
        in
          {Cst = map explode c0;
           Froz = map explode f0;
      	   Subst = map explode s0;
           Ar_c = ac0;
      	   Ar_f = af0}
  | ET_product lst ->
      list_it
      	(fun
      	   {Cst = c1; Froz = f1; Subst = s1; Ar_c = ac1; Ar_f = af1}
      	   {Cst = c2; Froz = f2; Subst = s2; Ar_c = ac2; Ar_f = af2}
      	 ->
      	   {Cst = c1 @ c2;
      	    Froz = f1 @ f2;
      	    Subst = s1 @ s2;
      	    Ar_c = ac1 + ac2;
      	    Ar_f = af1 + af2})
	(map passe_1 lst)
	empty_type
  | ET_constant (nm, param) ->
      {Cst = [((nm.Local_name, map passe_1 param), empty_type)];
       Froz = [];
       Subst = [];
       Ar_c = 1;
       Ar_f = 0}
  | ET_unit ->
      if !full_iso then
        empty_type
      else
        frozen_empty_type
;;

(* Compress bases *)
let rec compress2 =
  function
    [] -> []
  | (nm, arg)::l ->
      let rec filter =
        function
          [] -> ([], [])
        | ((nm2, arg2)::lst) as lst2 ->
            if nm2 = nm then
              let (lst_arg, rest) = filter lst in
        	(arg2::lst_arg, rest)
            else ([], lst2)
      in
        let (lst_arg, rest) = filter l in
          (nm, {Count = list_length lst_arg + 1; Parameter = arg::lst_arg})::(compress2 rest);;

let compress_var compare continue lst =
  compress2
    (sort__sort
       compare
       (map
          (function (nm, arg) -> (nm, continue arg))
          lst));;

let compress_cst_p compare continue lst =
  compress2
    (sort__sort
       compare
       (map
       	  (function ((nm, param), arg) -> (nm, map continue (arg::param)))
      	  lst));;

let compress_cst_s compare continue lst =
  compress2
    (sort__sort
       compare
       (map
       	  (function ((nm, param), arg) ->
             ((nm, map continue param), continue arg))
      	  lst));;

let normalize_entry_to_pattern (froz, subst, typ) =
  let rec passe_2
    {Cst = constant; Froz = frozen; Subst = subst; Ar_c = n1; Ar_f = n2}
  =
    {P_constant =
       compress_cst_p
       	 pattern_constant_less
         passe_2
         constant;
     P_frozen_variable =
       compress_var
       	 free_variable_less
         passe_2
         frozen;
     P_subst_variable =
       compress_var
       	 free_variable_less
         passe_2
         subst;
    P_arity = n1 + n2}
  in
    (froz, subst, passe_2 (passe_1 typ));;

let normalize_library_to_pattern (froz, _, typ) =
  let rec passe_2 {Cst = constant; Froz = frozen; Ar_c = n} =
    {P_constant =
       compress_cst_p
       	 pattern_constant_less
         passe_2
         constant;
     P_frozen_variable =
       [];
     P_subst_variable =
       compress_var
         free_variable_less
         passe_2
         frozen;
    P_arity = n}
  in
    (0, froz, passe_2 (passe_1 typ));;

let normalize_subject (froz, _, typ) =
  let rec passe_2
    {Cst = constant; Froz = frozen; Ar_c = n1; Ar_f = n2}
  =
    {S_constant =
       compress_cst_s
       	 subject_constant_less
         passe_2
         constant;
     S_frozen_variable =
       compress_var
       	 free_variable_less
         passe_2
         frozen;
    S_arity = n1 + n2}
  in
    (froz, passe_2 (passe_1 typ));;
