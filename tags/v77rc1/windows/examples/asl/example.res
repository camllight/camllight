>> 1;
   it : Number
   it = Numval 1

>> + 2 ((\x.x) 3);
   it : Number
   it = Numval 5

>> if + 0 1 then 1 else 0 fi;
   it : Number
   it = Numval 1

>> let f = \x. + x 1;
   f : (Number -> Number)
   f = Funval <fun>

>> let I = \x. x;
   I : ('a -> 'a)
   I = Funval <fun>

>> let x = I (f 2);
   x : Number
   x = Numval 3

>> let g = \x.x 1;
   g : ((Number -> 'a) -> 'a)
   g = Funval <fun>

>> + (I 1) (I I 2);
   it : Number
   it = Numval 3

>> (\x.x x) (\x.x);
   it : ('a -> 'a)
   it = Funval <fun>

>> (if x then (\x.x) else 2 fi) 0;
*** ASL Type clash between ('a -> 'a) and Number
*** Failed: ASL typing

>> let Z = \f. (\x.f(\y.x x y)) (\x.f(\y.x x y));
*** ASL Type clash between 'a and ('a -> 'b)
*** Failed: ASL typing

>> let Z = \f. (\V.V (magic V)) (\x.f(\y.magic x x y));
   Z : ((('a -> 'b) -> 'c) -> 'c)
   Z = Funval <fun>

>> let fact = Z (\fact.\n. if = n 0 then 1 else * n (fact (- n 1)) fi);
   fact : (Number -> Number)
   fact = Funval <fun>

>> fact 8;
   it : Number
   it = Numval 40320

>> let fib = Z (\fib.\n.
>>   if = n 1 then 1
>>   else if = n 2 then 1 else + (fib(- n 1)) (fib(- n 2)) fi fi
>> );
   fib : (Number -> Number)
   fib = Funval <fun>

>> fib 9;
   it : Number
   it = Numval 34

>> + (\x.x) 1;
*** ASL Type clash between Number and ('a -> 'a)
*** Failed: ASL typing

>> 
