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
#open "nat";;

type big_int = 
    { Sign : int; 
      Abs_Value : nat }
;;

let create_big_int sign nat =  
 if sign == 1 || sign == -1 ||
    (sign == 0 &&
     is_zero_nat nat 0 (num_digits_nat nat 0 (length_nat nat)))
 then { Sign = sign; 
         Abs_Value = nat }
  else invalid_arg "create_big_int"
;;

(* Sign of a big_int *)
let sign_big_int bi = bi.Sign;;

let zero_big_int =
 { Sign = 0;
   Abs_Value = make_nat 1 }
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
 { Sign = minus_int bi.Sign;
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
  if bi1.Sign == 0 && bi2.Sign == 0 then 0
  else if bi1.Sign < bi2.Sign then -1
  else if bi1.Sign > bi2.Sign then 1
  else if bi1.Sign == 1 then
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
    0 -> { Sign = -1; Abs_Value = nat_of_int 1}
  | 1 -> let size_bi = num_digits_big_int bi in
          let copy_bi = copy_nat (bi.Abs_Value) 0 size_bi in
            let _ = decr_nat copy_bi 0 size_bi 0 in
            { Sign = if is_zero_nat copy_bi 0 size_bi then 0 else 1;
              Abs_Value = copy_bi }
  | _ -> let size_bi = num_digits_big_int bi in
	 let size_res = succ (size_bi) in
         let copy_bi = create_nat (size_res) in
          blit_nat copy_bi 0 (bi.Abs_Value) 0 size_bi;
	  set_digit_nat copy_bi size_bi 0;
          let _ = incr_nat copy_bi 0 size_res 1 in
          { Sign = -1;
            Abs_Value = copy_bi }
;;

let succ_big_int bi =
 match bi.Sign with
    0 -> {Sign = 1; Abs_Value = nat_of_int 1}
  | -1 -> let size_bi = num_digits_big_int bi in
           let copy_bi = copy_nat (bi.Abs_Value) 0 size_bi in
            let _ = decr_nat copy_bi 0 size_bi 0 in
            { Sign = if is_zero_nat copy_bi 0 size_bi then 0 else -1;
              Abs_Value = copy_bi }
  | _ -> let size_bi = num_digits_big_int bi in
	 let size_res = succ (size_bi) in
         let copy_bi = create_nat (size_res) in
          blit_nat copy_bi 0 (bi.Abs_Value) 0 size_bi;
          set_digit_nat copy_bi size_bi 0;
	  let _ = incr_nat copy_bi 0 size_res 1 in
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
        -1 -> let res = create_nat (succ size_bi2) in
                (blit_nat res 0 (bi2.Abs_Value) 0 size_bi2; 
                 set_digit_nat res size_bi2 0;
                 let _ = add_nat res 0 (succ size_bi2)
                                (bi1.Abs_Value) 0 size_bi1 0 in
                 res)
       |_  -> let res = create_nat (succ size_bi1) in
               (blit_nat res 0 (bi1.Abs_Value) 0 size_bi1;
                set_digit_nat res size_bi1 0;
                let _ = add_nat res 0 (succ size_bi1)
                                (bi2.Abs_Value) 0 size_bi2 0 in
                res)}

  else      (* Subtract absolute values if signs are different *)
    match compare_nat (bi1.Abs_Value) 0 size_bi1 
                      (bi2.Abs_Value) 0 size_bi2 with
       0 -> zero_big_int
     | 1 -> { Sign = bi1.Sign;
               Abs_Value = 
                let res = copy_nat (bi1.Abs_Value) 0 size_bi1 in
                let _ = sub_nat res 0 size_bi1 
                                (bi2.Abs_Value) 0 size_bi2 1 in
                res }
     | _ -> { Sign = bi2.Sign;
              Abs_Value = 
               let res = copy_nat (bi2.Abs_Value) 0 size_bi2 in
               let _ = sub_nat res 0 size_bi2 
                              (bi1.Abs_Value) 0 size_bi1 1 in
               res }
;;

(* Coercion with int type *)
let big_int_of_int i =
  { Sign = sign_int i;
    Abs_Value =
      let res = (create_nat 1)
      in (if i == monster_int
             then (set_digit_nat res 0 biggest_int;
                   let _ = incr_nat res 0 1 1 in ())
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
            let _ = 
             mult_digit_nat res 0 size_res (bi.Abs_Value) 0 size_bi 
                            (nat_of_int biggest_int) 0 in
            { Sign = - (sign_big_int bi);
              Abs_Value = res }             
     else let res = make_nat (size_res) in
          let _ =
           mult_digit_nat res 0 size_res (bi.Abs_Value) 0 size_bi 
                          (nat_of_int (abs i)) 0 in
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
     let _ =
      if size_bi2 > size_bi1
      then mult_nat res 0 size_res (bi2.Abs_Value) 0 size_bi2 
                    (bi1.Abs_Value) 0 size_bi1
      else mult_nat res 0 size_res (bi1.Abs_Value) 0 size_bi1 
                    (bi2.Abs_Value) 0 size_bi2 in
     res }
;;

(* (quotient, rest) of the euclidian division of 2 big_int *)
let quomod_big_int bi1 bi2 =
 if bi2.Sign == 0 then raise Division_by_zero
 else
  let size_bi1 = num_digits_big_int bi1
  and size_bi2 = num_digits_big_int bi2 in
   match compare_nat (bi1.Abs_Value) 0 size_bi1 
                     (bi2.Abs_Value) 0 size_bi2 with
      -1 -> (* 1/2 -> 0, reste 1, -1/2 -> -1, reste 1 *)
             if bi1.Sign == -1
              then (big_int_of_int(-1), add_big_int bi2 bi1)
              else (big_int_of_int 0, bi1)
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
                    let _ = incr_nat q 0 size_q 1 in
                    q) }, 
                { Sign = 1;
                  Abs_Value = 
                   (let _ = sub_nat new_r 0 size_bi2 r 0 size_bi2 1 in
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
           0 -> bi1.Abs_Value
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

let int_of_big_int bi = 
  try bi.Sign * int_of_nat bi.Abs_Value
  with Failure _ ->
    if eq_big_int bi (big_int_of_int monster_int) 
    then monster_int 
    else failwith "int_of_big_int"
;;

let is_int_big_int bi = 
   is_nat_int (bi.Abs_Value) 0 (num_digits_big_int bi)
|| (bi.Sign == -1 && num_digits_big_int bi == 1 &&
    num_leading_zero_bits_in_digit (bi.Abs_Value) 0 >= 1);;

(* XL: le "1" provient de "pred (length_of_digit - length_of_int))" *)

(* Coercion with nat type *)
let nat_of_big_int bi = 
 if bi.Sign == -1
 then failwith "nat_of_big_int"
 else copy_nat (bi.Abs_Value) 0 (num_digits_big_int bi)
;;

let sys_big_int_of_nat nat off len =
 let length = num_digits_nat nat off len in 
    { Sign = if is_zero_nat nat off  length then 0 else 1;
      Abs_Value = copy_nat nat off length }
;;

let big_int_of_nat nat =
 sys_big_int_of_nat nat 0 (length_nat nat)
;;

(* Coercion with string type *)

let string_of_big_int bi =
  if bi.Sign == -1
  then "-" ^ string_of_nat bi.Abs_Value
  else string_of_nat bi.Abs_Value
;;

(* XL: j'ai puissamment simplifie "big_int_of_string", en virant
   la notation scientifique (123e6 ou 123.456e12). *)

let sys_big_int_of_string s ofs len =
  let (sign, nat) =
    match nth_char s ofs with
      `-` -> if len > 1
                then (-1, sys_nat_of_string 10 s (ofs+1) (len-1))
                else failwith "sys_big_int_of_string"
    | `+` -> if len > 1
                then (1, sys_nat_of_string 10 s (ofs+1) (len-1))
                else failwith "sys_big_int_of_string"
    | _ -> if len > 0
              then (1, sys_nat_of_string 10 s ofs len)
              else failwith "sys_big_int_of_string" in
  { Sign = if is_zero_nat nat 0 (length_nat nat) then 0 else sign;
    Abs_Value = nat };;

let big_int_of_string s =
  sys_big_int_of_string s 0 (string_length s)
;;

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
    and res2 = make_nat n
    and l = num_bits_int n - 2 in
    let p = ref (1 lsl l) in
      blit_nat res 0 power_base pmax 1;
      for i = l downto 0 do
        let len = num_digits_nat res 0 n in
        let len2 = min n (2 * len) in
        let succ_len2 = succ len2 in
        let _ = square_nat res2 0 len2 res 0 len in
        begin
         if n land !p > 0
         then (set_to_zero_nat res 0 len;
               let _ =
                mult_digit_nat res 0 succ_len2 
                               res2 0 len2 
                               power_base pmax in
               ())
          else blit_nat res 0 res2 0 len2
         end;
         set_to_zero_nat res2 0 len2;
         p := !p lsr 1
      done;
    if rem > 0
     then (let _ =
            mult_digit_nat res2 0 n 
                           res 0 n power_base (pred rem) in
           res2)
     else res
  end
;;

let power_int_positive_int i n = 
  match sign_int n with
    0 -> unit_big_int
  | -1 -> invalid_arg "power_int_positive_int"
  | _ -> let nat = power_base_int (abs i) n in
           { Sign = if i >= 0
                       then sign_int i 
                       else if n land 1 == 0
                               then 1 
                               else -1;
             Abs_Value = nat} 
;;

let power_big_int_positive_int bi n = 
  match sign_int n with
    0 -> unit_big_int
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
             let _ = square_nat res2 0 len2 res 0 len in
               (if n land !p > 0 
                   then (set_to_zero_nat res 0 len;
                         let _ =
                          mult_nat res 0 succ_len2 
                                   res2 0 len2 (bi.Abs_Value) 0 bi_len in
                         set_to_zero_nat res2 0 len2)
                   else blit_nat res 0 res2 0 len2;
                   set_to_zero_nat res2 0 len2);
               p := !p lsr 1
           done;
           {Sign = if bi.Sign >=  0
                      then bi.Sign 
                      else if n land 1 == 0
                              then 1 
                              else -1;
            Abs_Value = res} 
;;

let power_int_positive_big_int i bi = 
  match sign_big_int bi with
    0 -> unit_big_int
  | -1 -> invalid_arg "power_int_positive_big_int"
  | _ -> let nat = power_base_nat 
                     (abs i) (bi.Abs_Value) 0 (num_digits_big_int bi) in
           { Sign = if i >= 0
                       then sign_int i 
                       else if is_digit_odd (bi.Abs_Value) 0
                               then -1 
                               else 1;
             Abs_Value = nat } 
;;

let power_big_int_positive_big_int bi1 bi2 = 
  match sign_big_int bi2 with
    0 -> unit_big_int
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
             let nat = bi2.Abs_Value in
             let len = num_digits_nat res 0 res_len in
             let len2 = min res_len (2 * len) in
             let succ_len2 = succ len2 in
             let _ = square_nat res2 0 len2 res 0 len in
               land_digit_nat nat 0 (nat_of_int !p) 0;
               if is_zero_nat nat 0 len_bi2 
                  then (blit_nat res 0 res2 0 len2;
                        set_to_zero_nat res2 0 len2)
                  else (set_to_zero_nat res 0 len;
                        let _ =
                         mult_nat res 0 succ_len2 
                                  res2 0 len2 (bi1.Abs_Value) 0 bi1_len in
                        set_to_zero_nat res2 0 len2);
               p := !p lsr 1
           done;
           {Sign = if bi1.Sign >= 0
                      then bi1.Sign 
                      else if is_digit_odd (bi2.Abs_Value) 0
                              then -1 
                              else 1;
            Abs_Value = res} 
;;

(* base_power_big_int compute bi*base^n *)
let base_power_big_int base n bi =
  match sign_int n with
    0 -> bi
  | -1 -> let nat = power_base_int base (minus_int n) in
           let len_nat = num_digits_nat nat 0 (length_nat nat) 
           and len_bi = num_digits_big_int bi in
             if len_bi < len_nat then
               invalid_arg "base_power_big_int"
             else if len_bi == len_nat &&
                     compare_digits_nat (bi.Abs_Value) len_bi nat len_nat == -1
               then invalid_arg "base_power_big_int"
             else
               let copy = create_nat (succ len_bi) in
                      blit_nat copy 0 (bi.Abs_Value) 0 len_bi;
                      set_digit_nat copy len_bi 0;
                      div_nat copy 0 (succ len_bi) 
                              nat 0 len_nat;
                      if not (is_zero_nat copy 0 len_nat) 
                         then invalid_arg "base_power_big_int"
                         else { Sign = bi.Sign;
                                Abs_Value = copy_nat copy len_nat 1 }
  | _ -> let nat = power_base_int base n in
         let len_nat = num_digits_nat nat 0 (length_nat nat) 
         and len_bi = num_digits_big_int bi in
         let new_len = len_bi + len_nat in
         let res = make_nat new_len in
         let _ =
          if len_bi > len_nat
             then mult_nat res 0 new_len 
                           (bi.Abs_Value) 0 len_bi 
                           nat 0 len_nat
             else mult_nat res 0 new_len 
                           nat 0 len_nat 
                           (bi.Abs_Value) 0 len_bi in
         if is_zero_nat res 0 new_len
            then zero_big_int
            else create_big_int (bi.Sign) res
;;

(********************

let decimal_of_string s off_set length =
 let offset_limit = pred (add_int off_set + length) in
 let n = (index_char s `.` off_set) in
   (* s ends with a `.` *)
   if n == offset_limit then failwith "decimal_of_string" else
   if n == -1 || n > offset_limit
      then
       let n2 = (index_char s `e` off_set) in
        (* s ends with a `e` *)
        if n2 == offset_limit then failwith "decimal_of_string" else
        (* No `.` no `e` : regular integer *)
        if n2 == -1 || n2 > offset_limit
         then (sub_string s off_set length, 0)
         else (* integer e int *)
              let new_len = n2 - off_set in
                       (sub_string s off_set new_len,
                        int_of_string (sub_string s (succ n2) 
                        sys_int_of_string 
                           (base, s, (succ n2),
                            (pred (sub_int (length, new_len)))))
           (* '.' is well formed into the string *)
      else let len = pred (add_int (length, sub_int (off_set, n))) in
           let n2 = (index_char s `e` n) in
             (* s ends with a `e` *)
             if eq_int (n2,offset_limit) then failwith "decimal_of_string" else
             if eq_int (n2, -1) || gt_int (n2, offset_limit)
                then (* integer . integer *)
                     (let s' = raw_make_string (pred length) ` `
                      and new_len = sub_int (n, off_set) in
                           (blit_string s' 0 s off_set new_len);
                           (blit_string s' new_len s (succ n) len);
                           s', 
                      minus_int len)
                else (* integer . integer e int *)
                     (let s' = raw_make_string 
                                 (pred (sub_int (n2, off_set))) ` `
                      and new_len = sub_int (n, off_set) in
                        (blit_string s' 0 s off_set new_len);
                        (blit_string s' new_len s (succ n) 
                                        (pred (sub_int (n2, n))));
                           s',
                      sub_int (sys_int_of_string 
                                 (base, s, (succ n2),
                                  (sub_int (offset_limit, n2))),
                               pred (sub_int (n2, n))))
;;

let sys_simple_big_int_of_string (base, s, off_set, length) =
 if eq (nth_char (off_set, s), `-`)
  then 
   let nat = (sys_nat_of_string (base, s, succ off_set, pred length)) 
    in
    if is_zero_nat (nat, 0, length_nat nat) 
     then 
      { Sign = 0; 
        Abs_Value = nat }
    else 
     { Sign = -1; 
       Abs_Value = nat }
 else
  let nat = 
   if eq (nth_char (off_set, s), `+`)
    then
     sys_nat_of_string (base, s, succ off_set, pred length)
   else sys_nat_of_string (base, s, off_set, length)
  in
   if is_zero_nat (nat, 0, length_nat nat)
    then
     { Sign = 0;
       Abs_Value = nat}
   else
    { Sign = 1;
      Abs_Value = nat}
;;

let sys_sys_big_int_of_string (base, s, off, len) = 
  let (sbi, k) = decimal_of_string (base, s, off, len) in
  let len_sbi = length_string sbi in
    if lt_int (k, 0) 
       then begin
            for i = add_int (len_sbi, k) to (pred len_sbi) do
                       if nth_char (i, sbi) != `0` 
                          then failwith "sys_big_int_of_string"
                     done;
            simple_big_int_of_string (base, sbi, 0, add_int (len_sbi, k))
            end
       else base_power_big_int 
              (base, k, simple_big_int_of_string (base, sbi, 0, len_sbi))
;;

let big_int_of_string s = 
 sys_big_int_of_string (10, s, 0, (length_string s));;

************************)

(* Coercion with float type *)

let float_of_big_int bi = 
  float__float_of_string (string_of_big_int bi)
;;

(* XL: suppression de big_int_of_float et nat_of_float. *)

(* Other functions needed *)

(* Integer part of the square root of a big_int *)
let sqrt_big_int bi =
 match bi.Sign with 
   -1 -> invalid_arg "sqrt_big_int"
 | 0  ->  {Sign = 0;
           Abs_Value = make_nat (1)}
 |  _  -> {Sign = 1;
           Abs_Value = sqrt_nat (bi.Abs_Value) 0 (num_digits_big_int bi)}
;;

let square_big_int bi =
  let len_bi = num_digits_big_int bi in
  let len_res = 2 * len_bi in
  let res = make_nat len_res in
  let _ = square_nat res 0 len_res (bi.Abs_Value) 0 len_bi in
    { Sign = bi.Sign;
      Abs_Value = res }
;;

(* XL: on verra si elles sont vraiment necessaires. *)

(*****************

let nth_digit_big_int bi n = nth_digit_nat (bi.Abs_Value) n;;

(* 
   Leading digit of a big_int with radix 10 : 
   if bi = sigma for n = 0 to N of b_n*10^n , leading_digit_big_int bi = b_N 
*)
let leading_digit_big_int bi =
 nth_digit_big_int bi (pred (num_digits_big_int bi))
;;

******)

(* round off of the futur last digit (of the integer represented by the string
   argument of the function) that is now the previous one.
   if s contains an integer of the form (10^n)-1 
    then s <- only 0 digits and the result_int is true
   else s <- the round number and the result_int is false *)
let round_futur_last_digit s off_set length =
 let l = pred (length + off_set) in 
  if int_of_char(nth_char s l) >= int_of_char `5`
    then
     let rec round_rec l = 
      let current_char = nth_char s l in 
       if current_char == `9`
        then
         (set_nth_char s l `0`;
          if l == off_set then true else round_rec (pred l))
        else 
         (set_nth_char s l 
                       (char_of_int (succ (int_of_char current_char)));
          false)
     in round_rec (pred l)
   else false
;; 

(* Approximation with floating decimal point a` la approx_ratio_exp *)
let approx_big_int prec bi =
  let len_bi = num_digits_big_int bi in
  let n = 
    max 0
        (int_of_big_int (
          add_int_big_int 
            (minus_int prec) 
            (div_big_int (mult_big_int (big_int_of_int (pred len_bi)) 
                                      (big_int_of_string "963295986")) 
                        (big_int_of_string "100000000")))) in
  let s =
    string_of_big_int (div_big_int bi (power_int_positive_int 10 n)) in
  let (sign, off, len) = 
    if nth_char s 0 == `-`
       then ("-", 1, succ prec)
       else ("", 0, prec) in
  if (round_futur_last_digit s off (succ prec))
       then (sign^"1."^(make_string prec `0`)^"e"^
             (string_of_int (n + 1 - off + string_length s)))
       else (sign^(sub_string s off 1)^"."^
             (sub_string s (succ off) (pred prec))
             ^"e"^(string_of_int (n - succ off + string_length s)))
;;
