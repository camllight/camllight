#open "terms";;
#open "order";;
#open "kb";;

(* The axioms for groups *)

let group_rules = [
  1, (1, (Term("*", [Term("U",[]); Var 1]), Var 1));
  2, (1, (Term("*", [Term("I",[Var 1]); Var 1]), Term("U",[])));
  3, (3, (Term("*", [Term("*", [Var 1; Var 2]); Var 3]),
          Term("*", [Var 1; Term("*", [Var 2; Var 3])])))
];;

let group_precedence op1 op2 =
  if op1 = op2 then Equal else
  if (op1 = "I") or (op2 = "U") then Greater else NotGE
;;

let group_order = rpo group_precedence lex_ext 
;;

(* Another example *)

let geom_rules = [
 1,(1,(Term ("*",[(Term ("U",[])); (Var 1)]),(Var 1)));
 2,(1,(Term ("*",[(Term ("I",[(Var 1)])); (Var 1)]),(Term ("U",[]))));
 3,(3,(Term ("*",[(Term ("*",[(Var 1); (Var 2)])); (Var 3)]),
  (Term ("*",[(Var 1); (Term ("*",[(Var 2); (Var 3)]))]))));
 4,(0,(Term ("*",[(Term ("A",[])); (Term ("B",[]))]),
  (Term ("*",[(Term ("B",[])); (Term ("A",[]))]))));
 5,(0,(Term ("*",[(Term ("C",[])); (Term ("C",[]))]),(Term ("U",[]))));
 6,(0,
  (Term
   ("*",
    [(Term ("C",[]));
     (Term ("*",[(Term ("A",[])); (Term ("I",[(Term ("C",[]))]))]))]),
  (Term ("I",[(Term ("A",[]))]))));
 7,(0,
  (Term
   ("*",
    [(Term ("C",[]));
     (Term ("*",[(Term ("B",[])); (Term ("I",[(Term ("C",[]))]))]))]),
  (Term ("B",[]))))
];;

let geom_rank = function
  | "U" -> 0
  | "*" -> 1
  | "I" -> 2
  | "B" -> 3
  | "C" -> 4
  | "A" -> 5
  |  _  -> failwith "Geom_rank"
;;

let geom_precedence op1 op2 =
  let r1 = geom_rank op1
  and r2 = geom_rank op2 in
    if r1 = r2 then Equal else
    if r1 > r2 then Greater else NotGE
;;

let geom_order = rpo geom_precedence lex_ext 
;;

kb_complete (gt_ord group_order) [] group_rules;;

(* If you have a fast machine, you may uncomment the following line: *)

(* print_newline(); kb_complete (gt_ord geom_order) [] geom_rules;; *)

