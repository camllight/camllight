(* ratio: arithmetique exacte sur les rationnels.
   Derive de ratios.ml de Caml V3.1, Vale'rie Me'nissier. *)

#open "eq";;
#open "exc";;
#open "int";;
#open "bool";;
#open "ref";;
#open "fstring";;
#open "fchar";;
#open "int_misc";;
#open "string_misc";;
#open "nat";;
#open "big_int";;
#open "arith_flags";;

(* Definition of the type ratio :
   Conventions :                                 
   - the denominator is always a positive number 
   - the sign of n/0 is the sign of n            
These convention is automatically respected when a ratio is created with 
the create_ratio primitive
*)

type ratio =
 { mutable Numerator :   big_int;
   mutable Denominator : big_int;
   mutable Normalized : bool}
;;

let failwith_zero name = 
    let s = "infinite or undefined rational number" in
    failwith (if string_length name == 0 then s else name ^ " " ^ s);;

let numerator_ratio r = r.Numerator
and denominator_ratio r = r.Denominator
;;

let null_denominator r = sign_big_int r.Denominator == 0;;

let verify_null_denominator r = 
  if sign_big_int r.Denominator == 0
     then (if !error_when_null_denominator_flag
           then (failwith_zero "")
           else true)
     else false;;

let sign_ratio r = sign_big_int r.Numerator ;;

(* Physical normalization of rational numbers *)
(* 1/0, 0/0 and -1/0 are the normalized forms for n/0 numbers *)
let normalize_ratio r = 
  if r.Normalized then r
  else if verify_null_denominator r then begin
    r.Numerator <- big_int_of_int (sign_big_int r.Numerator);
    r.Normalized <- true;
    r
  end else begin
    let p = gcd_big_int r.Numerator r.Denominator in
    if eq_big_int p unit_big_int 
    then begin
      r.Normalized <- true; r
    end else begin
      r.Numerator <- div_big_int (r.Numerator) p; 
      r.Denominator <- div_big_int (r.Denominator) p; 
      r.Normalized <- true; r
    end
  end
;;

let cautious_normalize_ratio r =
 if (!normalize_ratio_flag) then (normalize_ratio r) else r
;;

let cautious_normalize_ratio_when_printing r =
 if (!normalize_ratio_when_printing_flag) then (normalize_ratio r) else r
;;

let create_ratio bi1 bi2 =
 match sign_big_int bi2 with 
       -1 -> cautious_normalize_ratio
               { Numerator = minus_big_int bi1; 
                 Denominator = minus_big_int bi2;
                 Normalized = false }
     | 0 -> if !error_when_null_denominator_flag 
                then (failwith_zero "create_ratio")
                else cautious_normalize_ratio 
                    { Numerator = bi1; Denominator = bi2; Normalized = false }
     | _ ->  cautious_normalize_ratio 
              { Numerator = bi1; Denominator = bi2; Normalized = false }
;;

let create_normalized_ratio bi1 bi2 =
 match sign_big_int bi2 with
  -1 -> { Numerator = minus_big_int bi1; 
          Denominator = minus_big_int bi2; 
          Normalized = true }
|  0 -> if !error_when_null_denominator_flag 
            then failwith_zero "create_normalized_ratio"
            else { Numerator = bi1; Denominator = bi2; Normalized = true }
|  _  -> { Numerator = bi1; Denominator = bi2; Normalized = true }
;;

let is_normalized_ratio r = r.Normalized
;;

let report_sign_ratio r bi =
  if sign_ratio r == -1
  then minus_big_int bi 
  else bi
;;

let abs_ratio r = 
 { Numerator = abs_big_int r.Numerator;
   Denominator = r.Denominator;
   Normalized = r.Normalized }
;;

let is_integer_ratio r =
 eq_big_int ((normalize_ratio r).Denominator) unit_big_int
;;

(* Operations on rational numbers *)

let add_ratio r1 r2 =
 if !normalize_ratio_flag then begin
    let p = gcd_big_int ((normalize_ratio r1).Denominator) 
                        ((normalize_ratio r2).Denominator) in
    if eq_big_int p unit_big_int then 
       {Numerator = add_big_int (mult_big_int (r1.Numerator) r2.Denominator) 
                                (mult_big_int (r2.Numerator) r1.Denominator);
        Denominator = mult_big_int (r1.Denominator) r2.Denominator;
        Normalized = true}
    else begin
      let d1 = div_big_int (r1.Denominator) p
      and d2 = div_big_int (r2.Denominator) p in
      let n = add_big_int (mult_big_int (r1.Numerator) d2) 
                          (mult_big_int d1 r2.Numerator) in
      let p' = gcd_big_int n p in
        { Numerator = div_big_int n p';
          Denominator = mult_big_int d1 (div_big_int (r2.Denominator) p');
          Normalized = true }
      end
 end else
  { Numerator = add_big_int (mult_big_int (r1.Numerator) r2.Denominator) 
                            (mult_big_int (r1.Denominator) r2.Numerator);
    Denominator = mult_big_int (r1.Denominator) r2.Denominator;
    Normalized = false }
;;

let minus_ratio r =
 { Numerator = minus_big_int (r.Numerator);
   Denominator = r.Denominator;
   Normalized = r.Normalized }
;;

let add_int_ratio i r = 
  cautious_normalize_ratio r;
  { Numerator = add_big_int (mult_int_big_int i r.Denominator) r.Numerator;
    Denominator = r.Denominator;
    Normalized = r.Normalized }
;;

let add_big_int_ratio bi r = 
  cautious_normalize_ratio r;
  { Numerator = add_big_int (mult_big_int bi r.Denominator) r.Numerator ;
    Denominator = r.Denominator;
    Normalized = r.Normalized }
;;

let sub_ratio r1 r2 = add_ratio r1 (minus_ratio r2)
;;

let mult_ratio r1 r2 =
 if !normalize_ratio_flag then begin
   let p1 = gcd_big_int ((normalize_ratio r1).Numerator) 
                        ((normalize_ratio r2).Denominator) 
   and p2 = gcd_big_int (r2.Numerator) r1.Denominator in
   let (n1, d2) = 
     if eq_big_int p1 unit_big_int 
         then (r1.Numerator, r2.Denominator)
         else (div_big_int (r1.Numerator) p1, div_big_int (r2.Denominator) p1)
   and (n2, d1) =
      if eq_big_int p2 unit_big_int 
         then (r2.Numerator, r1.Denominator)
         else (div_big_int r2.Numerator p2, div_big_int r1.Denominator p2) in
    { Numerator = mult_big_int n1 n2;
      Denominator = mult_big_int d1 d2;
      Normalized = true }
 end else
  { Numerator = mult_big_int (r1.Numerator) r2.Numerator;
    Denominator = mult_big_int (r1.Denominator) r2.Denominator;
    Normalized = false }
;;

let mult_int_ratio i r = 
 if !normalize_ratio_flag then
  begin
   let p = gcd_big_int ((normalize_ratio r).Denominator) (big_int_of_int i) in
   if eq_big_int p unit_big_int 
      then { Numerator = mult_big_int (big_int_of_int i) r.Numerator;
             Denominator = r.Denominator;
             Normalized = true }
      else { Numerator = mult_big_int (div_big_int (big_int_of_int i) p) 
                                      r.Numerator;
             Denominator = div_big_int (r.Denominator) p;
             Normalized = true }
  end
 else
  { Numerator = mult_int_big_int i r.Numerator;
    Denominator = r.Denominator;
    Normalized = false }
;;

let mult_big_int_ratio bi r = 
 if !normalize_ratio_flag then 
  begin
   let p = gcd_big_int ((normalize_ratio r).Denominator) bi in
     if eq_big_int p unit_big_int
        then { Numerator = mult_big_int bi r.Numerator;
               Denominator = r.Denominator;
               Normalized = true }
        else { Numerator = mult_big_int (div_big_int bi p) r.Numerator;
               Denominator = div_big_int (r.Denominator) p;
               Normalized = true }
  end
 else
  { Numerator = mult_big_int bi r.Numerator;
      Denominator = r.Denominator;
      Normalized = false }
;;

let square_ratio r =
  cautious_normalize_ratio r;
  { Numerator = square_big_int r.Numerator;
    Denominator = square_big_int r.Denominator;
    Normalized = r.Normalized }
;;

let inverse_ratio r =
  if !error_when_null_denominator_flag & eq_int (sign_big_int r.Numerator) 0
  then failwith_zero "inverse_ratio"
  else {Numerator = report_sign_ratio r r.Denominator; 
        Denominator = abs_big_int r.Numerator; 
        Normalized = r.Normalized}
;;

let div_ratio r1 r2 = 
  mult_ratio r1 (inverse_ratio r2)
;;

(* Integer part of a rational number *)
(* Odd function *)
let integer_ratio r = 
 if null_denominator r then failwith_zero "integer_ratio"
 else if sign_ratio r == 0 then zero_big_int
 else report_sign_ratio r (div_big_int (abs_big_int r.Numerator) 
                                       (abs_big_int r.Denominator))
;;

(* Floor of a rational number *)
(* Always less or equal to r *)
let floor_ratio r = 
 verify_null_denominator r;
 div_big_int (r.Numerator) r.Denominator
;;

(* Round of a rational number *)
(* Odd function, 1/2 -> 1 *)
let round_ratio r =
 verify_null_denominator r;
  let abs_num = abs_big_int r.Numerator in
   let bi = div_big_int abs_num r.Denominator in
    report_sign_ratio r 
     (if sign_big_int 
          (sub_big_int 
           (mult_int_big_int 
             2 
             (sub_big_int abs_num (mult_big_int (r.Denominator) bi))) 
           r.Denominator) == -1
      then bi
      else succ_big_int bi)
;;

let ceiling_ratio r = 
 if (is_integer_ratio r)
 then r.Numerator
 else succ_big_int (floor_ratio r)
;;


(* Comparison operators on rational numbers *)
let eq_ratio r1 r2 =
 normalize_ratio r1; 
 normalize_ratio r2;
 eq_big_int (r1.Numerator) r2.Numerator &
 eq_big_int (r1.Denominator) r2.Denominator 
;;

let compare_ratio r1 r2 =
  if verify_null_denominator r1 then
        let sign_num_r1 = sign_big_int r1.Numerator in
         if (verify_null_denominator r2)
          then 
           let sign_num_r2 = sign_big_int r2.Numerator in
             if sign_num_r1 == 1 & sign_num_r2 == -1 then  1 
             else if sign_num_r1 == -1 & sign_num_r2 == 1 then -1
             else 0
         else sign_num_r1
  else if verify_null_denominator r2 then
         minus_int (sign_big_int r2.Numerator)
  else match compare_int (sign_big_int r1.Numerator) 
                         (sign_big_int r2.Numerator) with
               1 -> 1
             | -1 -> -1
             | _ -> if eq_big_int (r1.Denominator) r2.Denominator 
                    then compare_big_int (r1.Numerator) r2.Numerator
                    else compare_big_int 
                            (mult_big_int (r1.Numerator) r2.Denominator) 
                            (mult_big_int (r1.Denominator) r2.Numerator)
;;     

let lt_ratio r1 r2 = compare_ratio r1 r2 < 0
and le_ratio r1 r2 = compare_ratio r1 r2 <= 0
and gt_ratio r1 r2 = compare_ratio r1 r2 > 0
and ge_ratio r1 r2 = compare_ratio r1 r2 >= 0
;;

let max_ratio r1 r2 = if lt_ratio r1 r2 then r2 else r1
and min_ratio r1 r2 = if gt_ratio r1 r2 then r2 else r1
;;

let eq_big_int_ratio bi r =
 (is_integer_ratio r) & eq_big_int bi r.Numerator;;

let compare_big_int_ratio bi r =
 normalize_ratio r;
 if (verify_null_denominator r)
 then minus_int (sign_big_int r.Numerator)
 else compare_big_int (mult_big_int bi r.Denominator) r.Numerator
;;

let lt_big_int_ratio bi r = compare_big_int_ratio bi r < 0
and le_big_int_ratio bi r = compare_big_int_ratio bi r <= 0
and gt_big_int_ratio bi r = compare_big_int_ratio bi r > 0
and ge_big_int_ratio bi r = compare_big_int_ratio bi r >= 0
;;

(* Coercions *)

(* Coercions with type int *)
let int_of_ratio r = 
 if ((is_integer_ratio r) & (is_int_big_int r.Numerator))
 then (int_of_big_int r.Numerator)
 else failwith "integer argument required"

and ratio_of_int i =
 { Numerator = big_int_of_int i; 
   Denominator = unit_big_int;
   Normalized = true }
;;

(* Coercions with type nat *)
let ratio_of_nat nat = 
 { Numerator = big_int_of_nat nat;
   Denominator = unit_big_int;
   Normalized = true }

and nat_of_ratio r =
 normalize_ratio r;
 if not (is_integer_ratio r) then
          failwith "nat_of_ratio"
 else if sign_big_int r.Numerator > -1 then
         nat_of_big_int (r.Numerator)
 else failwith "nat_of_ratio"
;;

(* Coercions with type big_int *)
let ratio_of_big_int bi = 
 { Numerator = bi; Denominator = unit_big_int; Normalized = true }

and big_int_of_ratio r =
 normalize_ratio r;
 if is_integer_ratio r 
  then r.Numerator
 else failwith "big_int_of_ratio"
;;

let div_int_ratio i r = 
  verify_null_denominator r;
  mult_int_ratio i (inverse_ratio r)
;;

let div_ratio_int r i = 
  div_ratio r (ratio_of_int i)
;;

let div_big_int_ratio bi r = 
  verify_null_denominator r;
  mult_big_int_ratio bi (inverse_ratio r)
;;

let div_ratio_big_int r bi = 
  div_ratio r (ratio_of_big_int bi)
;;

(* Functions on type string                                 *)
(* giving floating point approximations of rational numbers *)

let only_zeros s i l =
 let res = ref true in
 for j = i to i + l - 1 do
  if s.[j] <> `0` then res := false
 done;
 !res
;;

(* Position of the leading digit of the decimal expansion          *)
(* of a strictly positive rational number                          *)
(* if the decimal expansion of a non null rational r is equal to   *)
(* sigma for k=-P to N of r_k*10^k then msd_ratio r = N            *)
(* Nota : for a big_int we have msd_ratio = nums_digits_big_int -1 *)
let msd_ratio r =
 cautious_normalize_ratio r;
 if null_denominator r then failwith_zero "msd_ratio"
 else if sign_big_int r.Numerator == 0 then 0
 else begin
         let str_num = string_of_big_int r.Numerator
         and str_den = string_of_big_int r.Denominator in
         let size_num = string_length str_num
         and size_den = string_length str_den in
         let rec msd_rec str_num nnum str_den nden i m = 
           if i > nnum then
            if i > nden or only_zeros str_den i (nden - i)
            then m else pred m
           else if i > nden then m
           else match compare_int (int_of_char (nth_char str_num i)) 
                                  (int_of_char (nth_char str_den i)) with
                    0 -> msd_rec str_num nnum str_den nden (succ i) m
                  | 1 -> m
                  |  _ -> pred m
         in msd_rec str_num (pred size_num) str_den (pred size_den) 
                     0 (size_num - size_den)
      end
;;

(* Decimal approximations of rational numbers *)

(* Approximation with fix decimal point *)
(* This is an odd function and the last digit is round off *)
(* Format integer_part . decimal_part_with_n_digits *)
let approx_ratio_fix n r =
 (* Don't need to normalize *)
 if (null_denominator r) then failwith_zero "approx_ratio_fix"
 else
  let sign_r = sign_ratio r in 
   if sign_r == 0
   then "+0" (* r = 0 *)
   else (* r.Numerator and r.Denominator are not null numbers 
           s contains one more digit than desired for the round off operation
           and to have enough room in s when including the decimal point *)
    if n >= 0 then 
        let s = 
         let nat = 
           (nat_of_big_int
                (div_big_int
                   (base_power_big_int
                       10 (succ n) (abs_big_int r.Numerator))
                   r.Denominator))
         in (if sign_r == -1 then "-" else "+") ^ string_of_nat nat in
        let l = string_length s in
         if round_futur_last_digit s 1 (pred l) 
          then begin (* if one more char is needed in s *)
           let str = (make_string (succ l) `0`) in 
            set_nth_char str 0 (if sign_r == -1 then `-` else `+`);
            set_nth_char str 1 `1`;
            set_nth_char str (l - n) `.`;
            str
          end else (* s can contain the final result *)
           if l > n + 2
            then begin (* |r| >= 1, set decimal point *)
             let l2 = (pred l) - n in
               blit_string s l2 s (succ l2) n; 
               set_nth_char s l2 `.`; s
            end else begin (* |r| < 1, there must be 0-characters *)
                           (* before the significant development, *)
                           (* with care to the sign of the number *)
             let size = n + 3 in
             let m = size - l + 2
             and str = make_string size `0` in

              (blit_string (if sign_r == 1 then "+0." else "-0.") 0 str 0 3);
              (blit_string s 1 str m (l - 2));
              str
            end
       else begin
         let s = string_of_big_int
                   (div_big_int
                      (abs_big_int r.Numerator) 
                      (base_power_big_int
                        10 (minus_int n) r.Denominator)) in
         let len = succ (string_length s) in
         let s' = make_string len `0` in
          set_nth_char s' 0 (if sign_r == -1 then `-` else `+`);
          blit_string s 0 s' 1 (pred len);
          s'
       end
;;

(* Number of digits of the decimal representation of an int *)
let num_decimal_digits_int n = 
 string_length (string_of_int n);;

(* Approximation with floating decimal point *)
(* This is an odd function and the last digit is round off *)
(* Format (+/-)(0. n_first_digits e msd)/(1. n_zeros e (msd+1) *)
let approx_ratio_exp n r = 
 (* Don't need to normalize *)
 if (null_denominator r) then failwith_zero "approx_ratio_exp"
 else if n <= 0 then invalid_arg "approx_ratio_exp"
 else
  let sign_r = sign_ratio r
  and i = ref (n + 3) in
   if sign_r == 0
     then
      let s = make_string (n + 5) `0` in
       (blit_string "+0." 0 s 0 3);
       (blit_string "e0" 0 s !i 2); s
   else 
     let msd = msd_ratio (abs_ratio r) in
     let k = n - msd in
     let s = 
      (let nat = nat_of_big_int
                (if k < 0
                  then 
                   div_big_int (abs_big_int r.Numerator) 
                               (base_power_big_int 10 (minus_int k) 
                                                   r.Denominator)
                 else 
                  div_big_int (base_power_big_int
                                10 k (abs_big_int r.Numerator))
                               r.Denominator) in
       string_of_nat nat) in
     if (round_futur_last_digit s 0 (string_length s)) 
      then
       let m = num_decimal_digits_int (succ msd) in
       let str = make_string (n + m + 4) `0` in
         (blit_string (if sign_r == -1 then "-1." else "+1.") 0 str 0 3);
         set_nth_char str !i (`e`);
         incr i;
         (if m == 0
          then set_nth_char str !i `0` 
          else blit_string (string_of_int (succ msd)) 0 str !i m); 
         str
     else
      let m = num_decimal_digits_int (succ msd)
      and p = n + 3 in
      let str = make_string (succ (m + p)) `0` in
        (blit_string (if sign_r == -1 then "-0." else "+0.") 0 str 0 3);
        (blit_string s 0 str 3 n);
        set_nth_char str p `e`;
        (if m == 0
          then set_nth_char str (succ p) `0`
          else (blit_string (string_of_int (succ msd)) 0 str (succ p) m));
        str
;;

(* String approximation of a rational with a fixed number of significant *)
(* digits printed                                                        *)
let float_of_rational_string r = 
  let s = approx_ratio_exp !floating_precision r in
    if nth_char s 0 == `+`
       then (sub_string s 1 (pred (string_length s)))
       else s
;;

(* Coercions with type string *)
let string_of_ratio r = 
 cautious_normalize_ratio_when_printing r;
 if !approx_printing_flag
    then float_of_rational_string r
    else string_of_big_int r.Numerator ^ "/" ^ string_of_big_int r.Denominator
;;

(* XL: j'ai puissamment simplifie "ratio_of_string" en virant la notation
   scientifique. *)

let ratio_of_string s =
  let n = index_char s `/` 0 in
  if n == -1 then
    { Numerator = big_int_of_string s;
      Denominator = unit_big_int;
      Normalized = true }
  else
    create_ratio (sys_big_int_of_string s 0 n)
                 (sys_big_int_of_string s (n+1) (string_length s - n - 1))
;;

(* Coercion with type float *)

let float_of_ratio r =
  float__float_of_string (float_of_rational_string r)
;;

(* XL: suppression de ratio_of_float *)

let power_ratio_positive_int r n = 
  create_ratio (power_big_int_positive_int (r.Numerator) n) 
               (power_big_int_positive_int (r.Denominator) n)
;;

let power_ratio_positive_big_int r bi = 
  create_ratio (power_big_int_positive_big_int (r.Numerator) bi) 
               (power_big_int_positive_big_int (r.Denominator) bi)
;;
