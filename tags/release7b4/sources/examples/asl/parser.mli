(* $Id$ *)

#open "stream";;
#open "token";;

type asl = Const of int
         | Var of int
         | Cond of asl * asl * asl
         | App of asl * asl
         | Abs of string * asl
and top_asl = Decl of string * asl;;

exception Unbound of string;;

value init_env : string list;;
value global_env : string list ref;;

value top : token_type stream -> top_asl;;
value expr : token_type stream -> string list -> asl;;
value expr0 : token_type stream -> string list -> asl;;

value print_top : top_asl -> string stream;;
value print_expr : asl -> string stream;;

