(*************************************************************************)
(*                                                                       *)
(*                     Projet      Formel                                *)
(*                                                                       *)
(*                          CamlLight                                    *)
(*                                                                       *)
(*************************************************************************)
(*                                                                       *)
(*                            Inria                                      *)
(*                      Domaine de Voluceau                              *)
(*                      78150  Rocquencourt                              *)
(*                            France                                     *)
(*                                                                       *)
(*************************************************************************)

(* TypeRewrite.ml Rewrite gtypes to canonical form                       *)
(*		         Roberto Di Cosmo                		 *)

(*
#open "globals";;
*)
#open "myTypes";;

let full_iso = ref true;;

let rec unroll f =
  function
    [], t     -> t
  | a::r, t   -> f (Iarrow(a,unroll f (r,t)))
;;

let rec rew_type =
  function  (* rewrite : congruence closure *)
    Iproduct([x;y]) -> rew_type_irr (Iproduct([(rew_type x);(rew_type y)]))
  | Iarrow(x,y)     -> rew_type_irr (Iarrow((rew_type x),(rew_type y)))
  |          x   -> x

and rew_type_irr =
  function  (* rewrite : reduce expressions with irreducible subexpressions *)
    Iarrow(x,Unit) as t -> if !full_iso then Unit else t (* Eliminate unit types *)
  | Iarrow(Unit,x)      -> x
  | Iproduct([x;Unit])  -> x
  | Iproduct([Unit;x])  -> x
  | Iarrow(Iproduct(tyl),z) -> unroll rew_type_irr (tyl,z)
                                     (* currying can produce new currying redexes *)
  | Iarrow(x,Iproduct(tyl)) -> Iproduct(map (fun y -> (rew_type_irr (Iarrow(x,y)))) tyl)
  | x                    -> x (* done *);;


type Type_Coords == int * skel_typ list;;

let TypeRewrite t =
    let rec flatten = function
        Iproduct(ty_list) -> let rec accumulate = 
                                function
                                  []   -> (0,[])
                                | a::t -> let (lgta,la) = flatten a 
                                          and (lgtt,lt) = accumulate t  
                                          in  (lgta+lgtt, la @ lt)
                             in accumulate ty_list         
      |       x     -> (1,[x])
in  (flatten (rew_type t));;


type VarRenaming == (int * int) list;;

let rec fold f v l =
  match l with
    [] -> (v,[])
  | a::r -> let (v1,a1) = f v a 
            in let (v2,l2) = fold f v1 r
               in (v2, a1::l2);;


let split_vars start ((lgt:int), coords) =
    let rec shift_compact_vars (start,env) =
      function
        Iarrow(x1,x2) ->
          let (start1,env1),newx1 =  shift_compact_vars (start,env) x1
          in  let (start2,env2),newx2 =  shift_compact_vars (start1,env1) x2
              in (start2,env2), Iarrow(newx1,newx2)
      |  Iproduct(ty_list) ->
          let (start_env, new_ty_list) = fold shift_compact_vars (start,env) ty_list
          in (start_env), Iproduct(new_ty_list)
      |  Iconstr(cstr,ty_list) ->
          let (start_env, new_ty_list) = fold shift_compact_vars (start,env) ty_list
          in (start_env), Iconstr(cstr,new_ty_list)
      | Ivar(i) ->
          (try
            (start,env),Ivar(assq i env)
           with  Not_found ->
            (start+1,(i,start)::env),Ivar(start))
      | Unit    ->
          (start,env),Unit
    
    and shifter (start,envlist) coord =
        (let ((ends,env),newcoord) = shift_compact_vars (start,[]) coord
         in  (ends, env @ envlist),newcoord)
    
    in (let (ren,newcoords) = fold shifter (start, []) coords
         in ren,lgt,newcoords);;



(* Rewrite one type to split normal with variables starting from START *)
(*
let SplitTR start typ =
  let res = split_vars start (TypeRewrite typ)
  in if !common__debug then (let (_,_, tyl) = res in do_list myTypes__print_myType tyl;print_newline());
     res;;
*)
let SplitTR start typ = split_vars start (TypeRewrite typ);;

(* export just SplitTR and full_iso *)
