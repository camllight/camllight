(*************************************************************************)
(*                                                                       *)
(*                     Projet      Formel                                *)
(*                                                                       *)
(*                            CAML                                       *)
(*                                                                       *)
(*************************************************************************)
(*                                                                       *)
(*                            Inria                                      *)
(*                      Domaine de Voluceau                              *)
(*                      78150  Rocquencourt                              *)
(*                            France                                     *)
(*                                                                       *)
(*************************************************************************)

(* Equal.ml	Interface to search by isomorphism functions             *)
(*		Roberto Di Cosmo and Pierre Weis			 *)

#open "myTypes";;
#open "TypeRewrite";;
#open "LC_unify";;
#open "output";;


let rec findiso flat_list flat_coord = 
  match flat_list with 
    []        ->  (false,[],[])
  | (a::rest) ->  let (yn,ren) = unify_Left_Commutative flat_coord a
                  in if yn  then (true,rest,ren) 
                            else let (yn,rest2,ren2) = findiso rest flat_coord
                                 in (yn,a::rest2,ren2);;

let quadratic_test a b =
  let renaming = ref [] in
  let rec q_test = 
        function [] -> 
            (function [] -> Some !renaming
                  |   _  -> None)
        |   (a::rest_a) as l ->
            (function [] -> None
             | b::rest_b -> let (yn,rest,ren) = findiso l b 
                            in if yn 
                               then (renaming := ren @ !renaming;
                                     q_test rest rest_b)
                               else None)
   in renaming := []; q_test a b;;


let are_isos a =
  let ((nextvar_a,renaming_a),lgt_a,typ_coords_a) = SplitTR 1 a in
  let flat_coords_a = map flatten typ_coords_a
  in function b -> 
     let ((_,renaming_b),lgt_b,typ_coords_b) = SplitTR nextvar_a b
     in if (lgt_a <> lgt_b)
        then (None,renaming_a,renaming_b)
        else let flat_coords_b = map flatten typ_coords_b
             in  (quadratic_test flat_coords_a flat_coords_b,
                  renaming_a,renaming_b);;
let build_renaming ren_unif rena renb = 
  let rename_builder renunif rena (key,val) =
      (key,assoc (assoc val renunif) rena)
  in map (rename_builder ren_unif (map (function (x,y) -> (y,x)) rena)) renb;;

let rec rename_type  = function 
    []       -> (function x -> x)
  | renaming -> (function 
                    Iconstr(cnstr,l) -> Iconstr(cnstr,(map (rename_type renaming) l))
                 |  Iproduct(l)       -> Iproduct((map (rename_type renaming) l))
                 |  Iarrow(x1,x2)     -> Iarrow((rename_type renaming x1),(rename_type renaming x2))
                 |  Ivar(i)           -> Ivar((assoc i renaming))
                 |  Unit              -> Unit);;
                 

let filter_iso_to a = 
    let is_iso_to_a = are_isos a (* partial evaluation of are_isos *)
    in function ((_, _, (_, _, typ)) as val) ->
      match is_iso_to_a (squeeze_typ typ) with
        None,_,_ -> ()
      | Some ren_unif, rena_x, renb_y 
	-> print_string (string_of_value val); print_newline()
;;
