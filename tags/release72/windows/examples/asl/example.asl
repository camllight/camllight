1;
+ 2 ((\x.x) 3);
if + 0 1 then 1 else 0 fi;
let f = \x. + x 1;
let I = \x. x;
let x = I (f 2);
let g = \x.x 1;
+ (I 1) (I I 2);
(\x.x x) (\x.x);
(if x then (\x.x) else 2 fi) 0;
let Z = \f. (\x.f(\y.x x y)) (\x.f(\y.x x y));
let Z = \f. (\V.V (magic V)) (\x.f(\y.magic x x y));
let fact = Z (\fact.\n. if = n 0 then 1 else * n (fact (- n 1)) fi);
fact 8;
let fib = Z (\fib.\n.
  if = n 1 then 1
  else if = n 2 then 1 else + (fib(- n 1)) (fib(- n 2)) fi fi
);
fib 9;
+ (\x.x) 1;
