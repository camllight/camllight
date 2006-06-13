(* $Id$ *)

#open "parser";;

type semval = Numval of int
            | Funval of (semval -> semval);;
exception Illtyped;;
exception SemantBug of string;;
let init_semantics caml_fun =
    Funval
      (function Numval n ->
         Funval(function Numval m -> Numval(caml_fun n m)
                        | _ -> raise Illtyped)
              | _ -> raise Illtyped);;
let caml_function = function
  | "+" -> prefix +
  | "-" -> prefix -
  | "*" -> prefix *
  | "/" -> prefix /
  | "=" -> (fun n m -> if n=m then 1 else 0)
  | s -> raise (SemantBug "Unknown primitive");;
let init_sem =  map (fun x -> init_semantics(caml_function x))
                    init_env;;
let global_sem = ref init_sem;;
let rec nth n = function
  | []  -> raise (Failure "nth")
  | x::l -> if n=1 then x else nth (n-1) l;;
let rec semant rho =
  let rec sem = function
    | Const n -> Numval n
    | Var(n) -> nth n rho
    | Cond(e1,e2,e3) ->
        (match sem e1 with
         | Numval 0 -> sem e3
         | Numval n -> sem e2
         | _ -> raise Illtyped)
    | Abs(_,e') -> Funval(fun x -> semant (x::rho) e')
    | App(e1,e2) -> (match sem e1 with
                     | Funval f -> f (sem e2)
                     | _ -> raise Illtyped) in
  sem;;

let semant_asl = function Decl(s,e) ->
  semant !global_sem e
;;

let print_semval = function
| Numval n -> print_string "Numval "; print_int n
| Funval f -> print_string "Funval <fun>"
;;

(*
semantics (parse_top "f = \\x. + x 1;");;
semantics (parse_top "i = \\x. x;");;
semantics (parse_top "x = i (f 2);");;
semantics (parse_top "y = (C x (\\x.x) 2) 0;");;
semantics (parse_top "z = \\f.((\\x.f(\\y.(x x) y))(\\x.f(\\y.(x x) y)));");;
semantics (parse_top "f = z(\\f.(\\n. C (= n 0) 1 ( * n (f (- n 1)))));");;
semantics (parse_top "x = f 8;");;
semantics (parse_top
  "b = z(\\b.(\\n. C (= n 1) 1 (C (= n 2) 1 (+ (b(- n 1)) (b(- n 2))))));");;
semantics (parse_top "x = b 9;");;
*)
