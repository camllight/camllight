(* $Id$ *)

#open "asl";;
#open "parser";;

let rec nth n = function
     []  -> raise (Failure "nth")
  | x::l -> if n=1 then x else nth (n-1) l;;

type asl_type =
  Unknown
| Number
| TypeVar of vartype
| Arrow of asl_type * asl_type
and vartype = {
  Index         : int;
  mutable Value : asl_type
}
and asl_type_scheme = Forall of int list * asl_type
;;

exception TypingBug of string;;

let new_vartype, reset_vartypes =
  (* Generating and resetting unknowns *)
  let counter = ref 0 in
  (function () -> counter := !counter+1; {Index = !counter; Value = Unknown}),
  (function () -> counter := 0)
;;

let rec shorten t =
    match t with
     TypeVar {Index=_; Value=Unknown} -> t
   | TypeVar ({Index=_;
                Value=TypeVar {Index=_;
                               Value=Unknown} as tv}) -> tv
   | TypeVar ({Index=_; Value=TypeVar tv1} as tv2)
            -> tv2.Value <- tv1.Value; shorten t
   | TypeVar {Index=_; Value=t'} -> t'
   | Unknown -> raise (TypingBug "shorten")
   | t' -> t';;
exception TypeClash of asl_type * asl_type;;

let occurs {Index=n;Value=_} = occrec
  where rec occrec =
  function TypeVar{Index=m;Value=Unknown} -> (n=m)
         | TypeVar{Index=m;Value=t} -> (n=m) or (occrec t)
         | Number -> false
         | Arrow(t1,t2) -> (occrec t1) or (occrec t2)
         | Unknown -> raise (TypingBug "occurs")
;;

let rec unify (tau1,tau2) =
  match (shorten tau1, shorten tau2)
  with (* type variable n and type variable m *)
       (TypeVar({Index=n; Value=Unknown} as tv1) as t1),
       (TypeVar({Index=m; Value=Unknown} as tv2) as t2)
           -> if n=m then () else tv1.Value <- t2
     | (* type t1 and type variable *)
      t1, (TypeVar ({Index=_;Value=Unknown} as tv) as t2)
            -> if not(occurs tv t1) then tv.Value <- t1
               else raise (TypeClash (t1,t2))
     | (* type variable and type t2 *)
       (TypeVar ({Index=_;Value=Unknown} as tv) as t1), t2
            -> if not(occurs tv t2) then tv.Value <- t2
               else raise (TypeClash (t1,t2))
     | Number, Number -> ()
     | Arrow(t1,t2), (Arrow(t'1,t'2) as t)
            -> unify(t1,t'1); unify(t2,t'2)
     | (t1,t2) -> raise (TypeClash (t1,t2));;

let init_typing_env =
    map (function s ->
            Forall([],Arrow(Number,
                              Arrow(Number,Number))))
         init_env ;;

let global_typing_env = ref init_typing_env;;
let vars_of_type tau = vars [] tau
 where rec vars vs =
  function Number -> vs
         | TypeVar {Index=n; Value=Unknown}
                 -> if mem n vs then vs else n::vs
         | TypeVar {Index=_; Value= t} -> vars vs t
         | Arrow(t1,t2) -> vars (vars vs t1) t2
         | Unknown -> raise (TypingBug "vars_of_type");;

let unknowns_of_type (bv,t) =
    subtract (vars_of_type t) bv;;
let flat = it_list (prefix @) [];;

let unknowns_of_type_env env =
    flat (map (function Forall(gv,t) -> unknowns_of_type (gv,t)) env)
;;

let rec make_set = function
     []  -> []
  | x::l -> if mem x l then make_set l else x::make_set l
;;

let generalise_type (gamma, tau) =
  let genvars =
    make_set (subtract (vars_of_type tau) (unknowns_of_type_env gamma))
  in Forall(genvars, tau)
;;

let gen_instance (Forall(gv,tau)) = 
  (* We associate a new unknown to each generic variable *)
  let unknowns =
      map (function n -> n, TypeVar(new_vartype()))
          gv
  in ginstance tau
  where rec ginstance = function
        (TypeVar {Index=n; Value=Unknown} as t) ->
                    (try assoc n unknowns
                     with Not_found -> t)
      | TypeVar {Index=_; Value= t} -> ginstance t
      | Number -> Number
      | Arrow(t1,t2) -> Arrow(ginstance t1, ginstance t2)
      | Unknown -> raise (TypingBug "gen_instance")
;;
let rec asl_typing_expr gamma = type_rec
where rec type_rec = function
    Const _ -> Number
  | Var n ->
      let sigma =
        try nth n gamma
        with Failure _ -> raise (TypingBug "Unbound")
      in gen_instance sigma
  | Cond (e1,e2,e3) ->
      let t1 = unify(Number, type_rec e1)
      and t2 = type_rec e2 and t3 = type_rec e3
      in unify(t2, t3); t3
  | App((Abs(x,e2) as f), e1) -> (* LET case *)
      let t1 = type_rec e1 in
      let sigma = generalise_type (gamma,t1)
      in asl_typing_expr (sigma::gamma) e2
  | App(e1,e2) ->
      let u = TypeVar(new_vartype())
      in unify(type_rec e1,Arrow(type_rec e2,u)); u
  | Abs(x,e) ->
      let u = TypeVar(new_vartype()) in
      let s = Forall([],u)
      in Arrow(u,asl_typing_expr (s::gamma) e)
;;

let tvar_name n =
 (* Computes a name "'a", ... for type variables, *)
 (* given an integer n representing the position  *)
 (* of the type variable in the list of generic   *)
 (* type variables                                *)
 let rec name_of n =
    let q,r = (n / 26), (n mod 26) in
    let s = make_string 1 (char_of_int (96+r)) in
    if q=0 then s else (name_of q)^s
 in "'"^(name_of n)
;;

let print_type_scheme (Forall(gv,t)) =
 (* Prints a type scheme.               *)
 (* Fails when it encounters an unknown *)
 let names = (names_of (1,gv)
      where rec names_of = function
             (n,[]) -> []
           | (n,(v1::Lv)) -> (tvar_name n)
                           ::(names_of (n+1, Lv))) in
 let tvar_names = combine (rev gv,names) in
 let rec print_rec = function
      TypeVar{Index=n; Value=Unknown} ->
         let name =
             try assoc n tvar_names
             with Not_found ->
               raise (TypingBug "Non generic variable")
         in print_string name
    | TypeVar{Index=_;Value=t} -> print_rec t
    | Number -> print_string "Number"
    | Arrow(t1,t2) ->
           print_string "("; print_rec t1;
           print_string " -> "; print_rec t2;
           print_string ")"
    | Unknown -> raise (TypingBug "print_type_scheme")
  in
  print_rec t
;;

let typing (Decl(s,e)) =
  reset_vartypes();
  let tau =
    try asl_typing_expr !global_typing_env e
    with TypeClash(t1,t2) ->
      let vars = vars_of_type(t1) @ vars_of_type(t2) in
      print_string "*** ASL Type clash between ";
      print_type_scheme (Forall(vars, t1));
      print_string " and ";
      print_type_scheme (Forall(vars, t2));
      reset_vartypes();
      raise (Failure "ASL typing")
  in                    
  generalise_type([], tau)
;;

(*
global_env:=init_env;;
typing (parse_top "x=1;");;
typing (parse_top "y = + 2 ((\\x.x) 3);");;
typing (parse_top "z = C (+ 0 1) 1 0;");;
typing (parse_top "i = \\x.x;");;
typing (parse_top "t = + (i 1) (i i 2);");;
typing (parse_top "f = (\\x.x x) (\\x.x);");;
typing (parse_top "a = + (\\x.x) 1;");;
typing (parse_top "z = \\f.((\\x.f(\\z.(x x)z)) (\\x.f(\\z.(x x)z)));");;
global_env := `z`::init_env;
global_typing_env:=
    (Forall([1],
     Arrow(Arrow(TypeVar{Index=1;Value=Unknown},
                   TypeVar{Index=1;Value=Unknown}),
            TypeVar{Index=1;Value=Unknown})))
   ::init_typing_env;
();;
typing (parse_top "f = z(\\f.(\\n. C (= n 0) 1 ( * n (f (- n 1)))));");;
typing (parse_top "x = f 8;");;
typing (parse_top
  "b = z(\\b.(\\n. C (= n 1) 1 (C (= n 2) 1 (+ (b(- n 1)) (b(- n 2))))));");;
typing (parse_top "x = b 9;");;
*)
