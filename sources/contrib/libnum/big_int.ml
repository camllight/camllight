(* big_int: arithmetique exacte sur grands entiers signes.
   Derive de big_ints.ml de Caml V3.1, Vale'rie Me'nissier. *)

#open "eq";;
#open "exc";;
#open "int";;
#open "bool";;
#open "ref";;
#open "fstring";;
#open "fchar";;
#open "pair";;
#open "int_misc";;
#open "fnat";;

type big_int = 
    { Sign : int; 
      Abs_Value : nat__nat }
;;

(* Accessing Sign and Absolute value fields of a big_int *)
let sign_big_int bi = bi.Sign;;
let abs_value_big_int bi = bi.Abs_Value;;

let create_big_int sign nat =  
 if sign == 1 || sign == -1 ||
    (sign == 0 &&
     is_zero_nat nat 0 (num_digits_nat nat 0 (length_nat nat)))
 then { Sign = sign; 
        Abs_Value = nat }
 else invalid_arg "create_big_int"
;;

let zero_big_int =
 { Sign = 0;
   Abs_Value = nat_of_int 0 }
;;

let unit_big_int =  
  { Sign = 1;
    Abs_Value = nat_of_int 1 }
;;

(* Number of digits in a big_int *)
let num_digits_big_int bi = 
 num_digits_nat (bi.Abs_Value) 0 (length_nat bi.Abs_Value) ;;

(* Opposite of a big_int *)
let minus_big_int bi = 
 { Sign = - bi.Sign;
   Abs_Value = copy_nat (bi.Abs_Value) 0 (num_digits_big_int bi)}
;;

(* Absolute value of a big_int *)
let abs_big_int bi = 
 { Sign = if bi.Sign == 0 then 0 else 1;
   Abs_Value = copy_nat (bi.Abs_Value) 0 (num_digits_big_int bi)}
;;

(* Comparison operators on big_int *)

(* 
   compare_big_int (bi, bi2) = sign of (bi-bi2) 
   i.e. 1 if bi > bi2
        0 if bi = bi2
        -1 if bi < bi2
*)
let compare_big_int bi1 bi2 =
  if bi1.Sign == 0 && bi2.Sign == 0 then 0 else
  if bi1.Sign < bi2.Sign then -1 else
  if bi1.Sign > bi2.Sign then 1 else
  if bi1.Sign == 1 then
     compare_nat (bi1.Abs_Value) 0 (num_digits_big_int bi1) 
                 (bi2.Abs_Value) 0 (num_digits_big_int bi2)
  else
     compare_nat (bi2.Abs_Value) 0 (num_digits_big_int bi2) 
                 (bi1.Abs_Value) 0 (num_digits_big_int bi1)
;;

let eq_big_int bi1 bi2 = compare_big_int bi1 bi2 == 0
and le_big_int bi1 bi2 = compare_big_int bi1 bi2 <= 0
and ge_big_int bi1 bi2 = compare_big_int bi1 bi2 >= 0
and lt_big_int bi1 bi2 = compare_big_int bi1 bi2 < 0
and gt_big_int bi1 bi2 = compare_big_int bi1 bi2 > 0
;;

let max_big_int bi1 bi2 = if lt_big_int bi1 bi2 then bi2 else bi1
and min_big_int bi1 bi2 = if gt_big_int bi1 bi2 then bi2 else bi1
;;

(* Operations on big_int *)

let pred_big_int bi = 
 match bi.Sign with
 | 0 -> { Sign = -1; Abs_Value = nat_of_int 1}
 | 1 -> let size_bi = num_digits_big_int bi in
        let copy_bi = copy_nat (bi.Abs_Value) 0 size_bi in
        set_decr_nat copy_bi 0 size_bi 0;
        { Sign = if is_zero_nat copy_bi 0 size_bi then 0 else 1;
          Abs_Value = copy_bi }
 | _ -> let size_bi = num_digits_big_int bi in
        let size_res = succ (size_bi) in
        let copy_bi = create_nat (size_res) in
         blit_nat copy_bi 0 (bi.Abs_Value) 0 size_bi;
         set_digit_nat copy_bi size_bi 0;
         set_incr_nat copy_bi 0 size_res 1;
         { Sign = -1;
           Abs_Value = copy_bi }
;;

let succ_big_int bi =
 match bi.Sign with
 | 0 -> {Sign = 1; Abs_Value = nat_of_int 1}
 | -1 -> let size_bi = num_digits_big_int bi in
         let copy_bi = copy_nat (bi.Abs_Value) 0 size_bi in
         set_decr_nat copy_bi 0 size_bi 0;
         { Sign = if is_zero_nat copy_bi 0 size_bi then 0 else -1;
           Abs_Value = copy_bi }
 | _ -> let size_bi = num_digits_big_int bi in
        let size_res = succ (size_bi) in
        let copy_bi = create_nat (size_res) in
        blit_nat copy_bi 0 (bi.Abs_Value) 0 size_bi;
        set_digit_nat copy_bi size_bi 0;
        set_incr_nat copy_bi 0 size_res 1;
        { Sign = 1;
          Abs_Value = copy_bi }
;;

let add_big_int bi1 bi2 = 
 let size_bi1 = num_digits_big_int bi1
 and size_bi2 = num_digits_big_int bi2 in
  if bi1.Sign == bi2.Sign
   then    (* Add absolute values if signs are the same *)
    { Sign = bi1.Sign;
      Abs_Value = 
       match compare_nat (bi1.Abs_Value) 0 size_bi1 
                         (bi2.Abs_Value) 0 size_bi2 with
       | -1 -> let res = create_nat (succ size_bi2) in
                (blit_nat res 0 (bi2.Abs_Value) 0 size_bi2; 
                 set_digit_nat res size_bi2 0;
                 set_add_nat res 0 (succ size_bi2)
                                (bi1.Abs_Value) 0 size_bi1 0;
                 res)
       | _  -> let res = create_nat (succ size_bi1) in
               (blit_nat res 0 (bi1.Abs_Value) 0 size_bi1;
                set_digit_nat res size_bi1 0;
                set_add_nat res 0 (succ size_bi1)
                                (bi2.Abs_Value) 0 size_bi2 0;
                res)}

  else      (* Subtract absolute values if signs are different *)
    match compare_nat (bi1.Abs_Value) 0 size_bi1 
                      (bi2.Abs_Value) 0 size_bi2 with
    | 0 -> zero_big_int
    | 1 -> { Sign = bi1.Sign;
             Abs_Value = 
              let res = copy_nat (bi1.Abs_Value) 0 size_bi1 in
              set_sub_nat res 0 size_bi1 
                       (bi2.Abs_Value) 0 size_bi2 1;
              res }
    | _ -> { Sign = bi2.Sign;
             Abs_Value = 
              let res = copy_nat (bi2.Abs_Value) 0 size_bi2 in
              set_sub_nat res 0 size_bi2 
                       (bi1.Abs_Value) 0 size_bi1 1;
              res }
;;

(* Coercion with int type *)
let big_int_of_int i =
  { Sign = sign_int i;
    Abs_Value =
      let res = (create_nat 1)
      in (if i == monster_int
             then (set_digit_nat res 0 biggest_int;
                   set_incr_nat res 0 1 1)
             else set_digit_nat res 0 (abs i));
      res }
;;

let add_int_big_int i bi = add_big_int (big_int_of_int i) bi;;

let sub_big_int bi1 bi2 = add_big_int bi1 (minus_big_int bi2);;

(* Returns i * bi *)
let mult_int_big_int i bi =
 let size_bi = num_digits_big_int bi in
 let size_res = succ size_bi in
  if i == monster_int
     then let res = create_nat size_res in
            blit_nat res 0 (bi.Abs_Value) 0 size_bi;
            set_mult_digit_nat res 0 size_res (bi.Abs_Value) 0 size_bi 
                            (nat_of_int biggest_int) 0;
            { Sign = - (sign_big_int bi);
              Abs_Value = res }             
     else let res = make_nat (size_res) in
          set_mult_digit_nat res 0 size_res (bi.Abs_Value) 0 size_bi 
                          (nat_of_int (abs i)) 0;
          { Sign = (sign_int i) * (sign_big_int bi);
            Abs_Value = res } 
;;

let mult_big_int bi1 bi2 =
 let size_bi1 = num_digits_big_int bi1
 and size_bi2 = num_digits_big_int bi2 in
 let size_res = size_bi1 + size_bi2 in
 let res = make_nat (size_res) in
  { Sign = bi1.Sign * bi2.Sign;
    Abs_Value = 
      (if size_bi2 > size_bi1
       then set_mult_nat res 0 size_res (bi2.Abs_Value) 0 size_bi2 
                     (bi1.Abs_Value) 0 size_bi1
       else set_mult_nat res 0 size_res (bi1.Abs_Value) 0 size_bi1 
                     (bi2.Abs_Value) 0 size_bi2;
      res) }
;;

(* (quotient, rest) of the euclidian division of 2 big_int *)
let quomod_big_int bi1 bi2 =
 if bi2.Sign == 0 then raise Division_by_zero
 else
  let size_bi1 = num_digits_big_int bi1
  and size_bi2 = num_digits_big_int bi2 in
   match compare_nat (bi1.Abs_Value) 0 size_bi1 
                     (bi2.Abs_Value) 0 size_bi2 with
   | -1 -> (* 1/2 -> 0, reste 1, -1/2 -> -1, reste 1 *)
            if bi1.Sign == -1
             then (big_int_of_int(-1), add_big_int bi2 bi1)
             else (zero_big_int, bi1)
   | 0 -> (big_int_of_int (bi1.Sign * bi2.Sign), zero_big_int)
   | _ -> let bi1_negatif = bi1.Sign == -1 in 
          let size_q =
           if bi1_negatif 
            then succ (max (succ (size_bi1 - size_bi2)) 1)
            else max (succ (size_bi1 - size_bi2)) 1
          and size_r = succ (max size_bi1 size_bi2) 
           (* r is long enough to contain both quotient and remainder *)
           (* of the euclidian division *)
          in
           (* set up quotient, remainder *)
           let q = create_nat size_q
           and r = create_nat size_r in
           blit_nat r 0 (bi1.Abs_Value) 0 size_bi1;
           set_to_zero_nat r size_bi1 (size_r - size_bi1);

           (* do the division of |bi1| by |bi2|
              - at the beginning, r contains |bi1| 
              - at the end, r contains 
                * in the size_bi2 least significant digits, the remainder 
                * in the size_r-size_bi2 most significant digits, the quotient
             note the conditions for application of div_nat are verified here 
            *)
           div_nat r 0 size_r (bi2.Abs_Value) 0 size_bi2;

           (* separate quotient and remainder *)
           blit_nat q 0 r size_bi2 (size_r - size_bi2);
           let not_null_mod = not (is_zero_nat r 0 size_bi2) in

           (* correct the signs, adjusting the quotient and remainder *)
           if bi1_negatif && not_null_mod
            then 
             (* bi1<0, r>0, noting r for (r, size_bi2) the remainder,      *)
             (* we have |bi1|=q * |bi2| + r with 0 < r < |bi2|,            *)
             (* thus -bi1 = q * |bi2| + r                                  *)
             (* and bi1 = (-q) * |bi2| + (-r) with -|bi2| < (-r) < 0       *)
             (* thus bi1 = -(q+1) * |bi2| + (|bi2|-r)                      *)
             (* with 0 < (|bi2|-r) < |bi2|                                 *)
             (* so the quotient has for sign the opposite of the bi2'one   *)
             (*                 and for value q+1                          *)
             (* and the remainder is strictly positive                     *)
             (*                  has for value |bi2|-r                     *)
             (let new_r = copy_nat (bi2.Abs_Value) 0 size_bi2 in
                     (* new_r contains (r, size_bi2) the remainder *)
               { Sign = - bi2.Sign;
                 Abs_Value =
                  (set_digit_nat q (pred size_q) 0;
                   set_incr_nat q 0 size_q 1;
                   q) }, 
               { Sign = 1;
                 Abs_Value = 
                  (set_sub_nat new_r 0 size_bi2 r 0 size_bi2 1;
                   new_r) })
            else 
             (if bi1_negatif then set_digit_nat q (pred size_q) 0; 
               { Sign = if is_zero_nat q 0 size_q 
                         then 0 
                         else bi1.Sign * bi2.Sign;
                 Abs_Value = q },
               { Sign = if not_null_mod then 1 else 0;
                 Abs_Value = copy_nat r 0 size_bi2 })
;;

let div_big_int bi1 bi2 = fst (quomod_big_int bi1 bi2)
and mod_big_int bi1 bi2 = snd (quomod_big_int bi1 bi2)
;;

let gcd_big_int bi1 bi2 =
 let size_bi1 = num_digits_big_int bi1 
 and size_bi2 = num_digits_big_int bi2 in
  if is_zero_nat (bi1.Abs_Value) 0 size_bi1 then abs_big_int bi2
  else if is_zero_nat (bi2.Abs_Value) 0 size_bi2 then
        { Sign = 1;
          Abs_Value = bi1.Abs_Value }
  else
        { Sign = 1;
          Abs_Value = 
           match compare_nat (bi1.Abs_Value) 0 size_bi1 
                             (bi2.Abs_Value) 0 size_bi2 with
           | 0 -> bi1.Abs_Value
           | 1 ->
              let res = copy_nat (bi1.Abs_Value) 0 size_bi1 in
              let len = 
                gcd_nat res 0 size_bi1 (bi2.Abs_Value) 0 size_bi2 in
              copy_nat res 0 len
           | _ ->
              let res = copy_nat (bi2.Abs_Value) 0 size_bi2 in
              let len = 
                gcd_nat res 0 size_bi2 (bi1.Abs_Value) 0 size_bi1 in
              copy_nat res 0 len
        }
;;

(* Coercion operators *)

let monster_big_int = big_int_of_int monster_int;;

let monster_nat = monster_big_int.Abs_Value;;

let is_int_big_int bi =
  num_digits_big_int bi == 1 &&
  match compare_nat bi.Abs_Value 0 1 monster_nat 0 1 with
  | 0 -> bi.Sign == -1
  | -1 -> true
  | _ -> false;;

let int_of_big_int bi =
  try let n = int_of_nat bi.Abs_Value in
      if bi.Sign = -1 then minus_int n else n
  with Failure _ ->
    if eq_big_int bi monster_big_int then monster_int
    else failwith "int_of_big_int"
;;

(* Coercion with nat type *)
let nat_of_big_int bi = 
 if bi.Sign == -1
 then failwith "nat_of_big_int"
 else copy_nat (bi.Abs_Value) 0 (num_digits_big_int bi)
;;

let sys_big_int_of_nat nat off len =
 let length = num_digits_nat nat off len in 
 { Sign = if is_zero_nat nat off length then 0 else 1;
   Abs_Value = copy_nat nat off length }
;;

let big_int_of_nat nat =
 sys_big_int_of_nat nat 0 (length_nat nat)
;;

(* Coercion with string type *)

let sys_string_of_big_int base before bi after =  
 sys_string_of_nat base 
  (if bi.Sign == -1 then before ^ "-" else before)
  bi.Abs_Value 0 (num_digits_big_int bi) after
;;

let string_of_big_int bi =
 sys_string_of_big_int 10 "" bi ""
;;

let string_for_read_of_big_int bi = sys_string_of_big_int 10 "#(" bi ")";;

let power_base_nat base nat off len =
  if is_zero_nat nat off len then nat_of_int 1 else
  let power_base = make_nat (succ length_of_digit) in
  let (pmax, pint) = make_power_base base power_base in
  let (n, rem) = 
      let (x, y) = quomod_big_int (sys_big_int_of_nat nat off len) 
                                  (big_int_of_int (succ pmax)) in
        (int_of_big_int x, int_of_big_int y) in       
  if n == 0 then copy_nat power_base (pred rem) 1 else
   begin
    let res = make_nat n
    and res2 = make_nat (succ n)
    and l = num_bits_int n - 2 in
    let p = ref (1 lsl l) in
      blit_nat res 0 power_base pmax 1;
      for i = l downto 0 do
        let len = num_digits_nat res 0 n in
        let len2 = min n (2 * len) in
        let succ_len2 = succ len2 in
        set_square_nat res2 0 len2 res 0 len;
        begin
         if n land !p > 0
         then (set_to_zero_nat res 0 len;
               set_mult_digit_nat res 0 succ_len2 
                               res2 0 len2 
                               power_base pmax)
          else blit_nat res 0 res2 0 len2
         end;
         set_to_zero_nat res2 0 len2;
         p := !p lsr 1
      done;
    if rem > 0
     then (set_mult_digit_nat res2 0 (succ n)
                              res 0 n
                              power_base (pred rem);
           res2)
     else res
  end
;;

let power_int_positive_int i n = 
  match sign_int n with
  | 0 -> unit_big_int
  | -1 -> invalid_arg "power_int_positive_int"
  | _ -> let nat = power_base_int (abs i) n in
           { Sign = if i >= 0 then sign_int i else
                    if n land 1 == 0 then 1 else -1;
             Abs_Value = nat} 
;;

let power_big_int_positive_int bi n = 
  match sign_int n with
  | 0 -> unit_big_int
  | -1 -> invalid_arg "power_big_int_positive_int"
  | _ -> let bi_len = num_digits_big_int bi in
         let res_len = bi_len * n in
         let res = make_nat res_len 
         and res2 = make_nat res_len 
         and l = num_bits_int n - 2 in
         let p = ref (1 lsl l) in
           blit_nat res 0 (bi.Abs_Value) 0 bi_len;
           for i = l downto 0 do
             let len = num_digits_nat res 0 res_len in
             let len2 = min res_len (2 * len) in
             let succ_len2 = succ len2 in
             set_square_nat res2 0 len2 res 0 len;
               (if n land !p > 0 
                   then (set_to_zero_nat res 0 len;
                         set_mult_nat res 0 succ_len2 
                                   res2 0 len2 (bi.Abs_Value) 0 bi_len;
                         set_to_zero_nat res2 0 len2)
                   else blit_nat res 0 res2 0 len2;
                   set_to_zero_nat res2 0 len2);
               p := !p lsr 1
           done;
           {Sign = if bi.Sign >= 0 then bi.Sign else
                   if n land 1 == 0 then 1 else -1;
            Abs_Value = res} 
;;

let power_int_positive_big_int i bi = 
  match sign_big_int bi with
  | 0 -> unit_big_int
  | -1 -> invalid_arg "power_int_positive_big_int"
  | _ -> let nat = power_base_nat 
                     (abs i) (bi.Abs_Value) 0 (num_digits_big_int bi) in
           { Sign = if i >= 0 then sign_int i else
                    if is_digit_odd (bi.Abs_Value) 0 then -1 else 1;
             Abs_Value = nat } 
;;

let power_big_int_positive_big_int bi1 bi2 = 
  match sign_big_int bi2 with
  | 0 -> unit_big_int
  | -1 -> invalid_arg "power_big_int_positive_big_int"
  | _ -> let nat = bi2.Abs_Value
         and off = 0 
         and len_bi2 = num_digits_big_int bi2 in
         let bi1_len = num_digits_big_int bi1 in
         let res_len = int_of_big_int (mult_int_big_int bi1_len bi2) in
         let res = make_nat res_len 
         and res2 = make_nat res_len 
         and l = (len_bi2 * length_of_digit 
                  - num_leading_zero_bits_in_digit nat (pred len_bi2)) - 2 in
         let p = ref (1 lsl l) in
           blit_nat res 0 (bi1.Abs_Value) 0 bi1_len;
           for i = l downto 0 do
             let nat = copy_nat bi2.Abs_Value 0 len_bi2 in
             let len = num_digits_nat res 0 res_len in
             let len2 = min res_len (2 * len) in
             let succ_len2 = succ len2 in
               set_square_nat res2 0 len2 res 0 len;
               land_digit_nat nat 0 (nat_of_int !p) 0;
               if is_zero_nat nat 0 len_bi2 
                  then (blit_nat res 0 res2 0 len2;
                        set_to_zero_nat res2 0 len2)
                  else (set_to_zero_nat res 0 len;
                        set_mult_nat res 0 succ_len2 
                                  res2 0 len2 (bi1.Abs_Value) 0 bi1_len;
                        set_to_zero_nat res2 0 len2);
               p := !p lsr 1
           done;
           {Sign = if bi1.Sign >= 0 then bi1.Sign else
                   if is_digit_odd (bi2.Abs_Value) 0 then -1 else 1;
            Abs_Value = res} 
;;

let base_power_big_int base n bi =
 if n = 0 then bi else
 let abs_val = base_power_nat base n bi.Abs_Value in
 { Sign = bi.Sign;
   Abs_Value = abs_val };;

let big_int_of_string_gen nat_of base s off_set length =
 let start_char = s.[off_set] in 
 let abs_val =
  match start_char with
  | `-` | `+` -> nat_of base s (succ off_set) (pred length)
  | _ -> nat_of base s off_set length in
 if is_zero_nat abs_val 0 (length_nat abs_val) then zero_big_int else
 let sgn = if start_char == `-` then -1 else 1 in
 {Sign = sgn; Abs_Value = abs_val}
;;

(* La chaîne s contient un entier signé sans notation scientifique:
   on appelle la fonction correspondante des nat. *)
let simple_big_int_of_string =
 big_int_of_string_gen simple_sys_nat_of_string;;

(* La chaîne s contient un entier signé avec notation scientifique:
   on appelle la fonction générale des nat. *)
let sys_big_int_of_string = big_int_of_string_gen sys_nat_of_string;;

(* Spécialisé à la base 10. *)
let big_int_of_string s = sys_big_int_of_string 10 s 0 (string_length s);;

(* Coercion with float type *)

let float_of_big_int bi = 
  float__float_of_string (string_of_big_int bi)
;;

let big_int_of_float f = big_int_of_string (float__string_of_float f)
;;

(* Other functions needed *)

let nth_digit_big_int bi n = nth_digit_nat (bi.Abs_Value) n;;

(* 
   Leading digit of a big_int with radix 10 : 
   if bi = sigma for n = 0 to N of b_n*10^n , leading_digit_big_int bi = b_N 
*)
let leading_digit_big_int bi =
 nth_digit_big_int bi (pred (num_digits_big_int bi))
;;

(* Integer part of the square root of a big_int *)
let sqrt_big_int bi =
 match bi.Sign with 
 | 0 -> zero_big_int
 | -1 -> invalid_arg "sqrt_big_int"
 | _ -> {Sign = 1;
         Abs_Value = sqrt_nat (bi.Abs_Value) 0 (num_digits_big_int bi)}
;;

let square_big_int bi =
  if bi.Sign == 0 then zero_big_int else
  let len_bi = num_digits_big_int bi in
  let len_res = 2 * len_bi in
  let res = make_nat len_res in
  set_square_nat res 0 len_res (bi.Abs_Value) 0 len_bi;
  {Sign = 1; Abs_Value = res}
;;

(* round off of the futur last digit (of the integer represented by the string
   argument of the function) that is now the previous one.
   if s contains an integer of the form (10^n)-1 
    then s <- only 0 digits and the result_int is true
   else s <- the round number and the result_int is false *)
let round_futur_last_digit s off_set length =
 let l = pred (length + off_set) in 
  if int_of_char s.[l] >= int_of_char `5`
    then
     let rec round_rec l = 
      let current_char = s.[l] in 
       if current_char == `9`
        then
         (s.[l] <- `0`;
          if l == off_set then true else round_rec (pred l))
        else 
         (s.[l] <- (char_of_int (succ (int_of_char current_char)));
          false)
     in round_rec (pred l)
   else false
;; 

(* Approximation with floating decimal point a` la approx_ratio_exp
   Proposition: l'exposant est la partie entière du log en base 10 du
   nombre. On obtient une approximation de ce nombre à partir de la
   partie entière du logarithme en base 2^32 du nombre en multipliant
   par le facteur de conversion correspondant: 32 * log 2 / log 10
   (environ 9.63295986125, qu'on obtient par division de 963295986 par
   100000000).
   On augmente la portabilité en introduisant length_of_digit.
*)

let exp_cv_factor_den, exp_cv_factor_num =
  let exp_cv_factor = float_of_int length_of_digit *. log 2.0 /. log 10.0 in
  let denominator = 100000000.0 in
  let numerator =
   big_int_of_string
    (string_of_float (floor (exp_cv_factor *. denominator))) in
  numerator, big_int_of_float denominator;;

let approx_big_int prec bi =
  let len_bi = num_digits_big_int bi in
  let n = 
    max 0
        (int_of_big_int (
          add_int_big_int 
            (- prec) 
            (div_big_int (mult_big_int (big_int_of_int (pred len_bi)) 
                                       exp_cv_factor_num) 
                         (exp_cv_factor_den)))) in
  let s =
    string_of_big_int (div_big_int bi (power_int_positive_int 10 n)) in
  let (sign, off, len) = 
    if s.[0] == `-`
       then ("-", 1, succ prec)
       else ("", 0, prec) in
  if round_futur_last_digit s off (succ prec)
       then (sign ^ "1." ^ make_string prec `0` ^ "e" ^
             string_of_int (n + 1 - off + string_length s))
       else (sign ^ sub_string s off 1 ^ "." ^
             sub_string s (succ off) (pred prec) ^ "e" ^
             string_of_int (n - succ off + string_length s))
;;

(* Big_int printing *)

let sys_print_big_int base before bi after =
  sys_print_nat 
    base (if bi.Sign = -1 then before^"-" else before)
    bi.Abs_Value 0 (num_digits_big_int bi) after
;;

let print_big_int bi = sys_print_big_int 10 "" bi "";;

let print_big_int_for_read bi = sys_print_big_int 10 "#(" bi ")";;

