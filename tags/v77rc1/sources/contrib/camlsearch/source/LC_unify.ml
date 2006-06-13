(*************************************************************************)
(*                                                                       *)
(*                     Projet      Formel                                *)
(*                                                                       *)
(*                         CamlLight                                     *)
(*                                                                       *)
(*************************************************************************)
(*                                                                       *)
(*                            Inria                                      *)
(*                      Domaine de Voluceau                              *)
(*                      78150  Rocquencourt                              *)
(*                            France                                     *)
(*                                                                       *)
(*************************************************************************)

(* LC_unify.ml	Unification modulo left commutativity                    *)
(*		Roberto Di Cosmo                 			 *)

#open "myTypes";;

(* type 'a option =
  None
| Some of 'a;;
*)

(*
type Head_type == int * skel_typ;; (* record the length of premisses lists *)

type Flat_type = Fl of Head_type * Flat_type list;;
*)

let rec flatten = function
    Iarrow(x,Iarrow(y,z)) -> 
        let nfx = flatten x and nfy = flatten y
        and (Fl ((lgth,exp), chain)) = flatten z
        in Fl ((lgth+2,exp), nfx::nfy::chain)
  | Iarrow(x,y)           -> Fl ((1,y),[flatten x])
  | y                     -> Fl ((0,y),[]);;
(* Standard code for computing permutations of a list *)

let rec perms = function
    [] -> [[]]
  | x -> list_it (function a -> function z1 ->
                   (list_it (function y -> function z2 -> (a::y)::z2)
                            (perms (except a x)) z1
                   )
                 ) x [];;

type VarRenaming == (int * int) list;;

let var_renaming = ref ([]:VarRenaming);;


let bound var = try (Some(assoc var !var_renaming)) with Not_found -> None;;

let rename_var (var1,var2) = 
    match bound var1, bound var2
    with Some bind1, Some bind2 -> 
           bind1 == var2 (* test consistency of new binding *)
      |  None, None ->
           var_renaming := (var1,var2)::(var2,var1)::!var_renaming; true
      |   _   -> false;;

exception UNIFYLIST;;
exception unifyHead;;
exception Found;;

let unify_Left_Commutative flat1 flat2 =

let rec unify_LC (Fl((n1,mlt1),ml_flat_list1)) (Fl((n2,mlt2),ml_flat_list2)) =
    if not (n1 == n2)
    then false    (* Flat types of different length are not unifiable  *)
    else let saved_var_renaming = !var_renaming       (* save partial renaming  for backtracking *)
         in if unify_head (mlt1,mlt2) then            (* set  up  the correct  associative links *)
            unify_list(ml_flat_list1, ml_flat_list2)
            else (var_renaming := saved_var_renaming; false) (* restore partial renaming *)
and unify_head heads = (* unify_head is a predicate with side effects on var_renaming *)
  (
   (try unif_rec heads;true with unifyHead -> false)
    where rec unif_rec = function
      Ivar(r),Ivar(s) -> if rename_var (r,s) then () else raise unifyHead
    |      _ ,Ivar _  -> raise unifyHead
    | Ivar _ , _      -> raise unifyHead
    | Iarrow(x1,x2),Iarrow(y1,y2)   -> if x1==x2 
                                      then do_list unif_rec ([x1,y1;x2,y2]) 
                                      else raise unifyHead
    | Iproduct(l1),Iproduct(l2)     -> if list_length l1 == list_length l2
                                       then do_list unif_rec (combine(l1,l2))
                                       else raise unifyHead
    | Iconstr(s1,l1),Iconstr(s2,l2) -> if isos_same_type_constr s1 s2 
                                        && list_length l1 == list_length l2
                                       then do_list unif_rec (combine(l1,l2)) 
                                       else raise unifyHead
    | Unit,Unit                     -> ()
    |       _       ,    _          -> raise unifyHead
   )

and unify_list = function (a,b) ->
     (try do_list
        (function x -> 
          let saved_var_renaming = !var_renaming in    (* save   renaming for backtracking  *)
          let _ = unify_map (x, b) in                  (* try to unify componentwise  ...   *)
          var_renaming := saved_var_renaming; ()       (* restore renaming for backtracking *)
         ) (perms a); false
     with Found -> true)
     where rec unify_map = function
          ([],[]) -> raise Found
        | (a::resta,b::restb) -> if unify_LC a b
                               then unify_map (resta,restb) else false
        | _ -> raise UNIFYLIST (* incorrect state: should never be executed *)


in 
    var_renaming:=[];
    let unifiable = unify_LC flat1 flat2 in
    (unifiable, !var_renaming)
;;

