(* nat: fonctions auxiliaires et d impression pour le type nat.
   Derive de nats.ml de Caml V3.1, Valerie Menissier.
   Adapte a Caml Light par Xavier Leroy & Pierre Weis.
   Portage 64 bits: Pierre Weis. *)

#open "eq";;
#open "exc";;
#open "int";;
#open "bool";;
#open "ref";;
#open "fstring";;
#open "fvect";;
#open "fchar";;
#open "int_misc";;

(* Nat temporaries *)
let tmp_A_2 = create_nat 2
and tmp_A_1 = create_nat 1 
and tmp_B_2 = create_nat 2 
;;

(* Sizes of words and strings. *)

let length_of_digit = sys__word_size;;

let make_nat len =
  if len <= 0 then invalid_arg "make_nat" else
    let res = create_nat len in set_to_zero_nat res 0 len; res
;;

let copy_nat nat off_set length =
 let res = create_nat (length) in
  blit_nat res 0 nat off_set length; 
  res
;;

let zero_nat = make_nat 1;;

let is_zero_nat n off len =
  compare_nat zero_nat 0 1 n off (num_digits_nat n off len) == 0 
;;

let is_nat_int nat off len =
  num_digits_nat nat off len == 1 && is_digit_int nat off
;;

let sys_int_of_nat nat off len =
  if is_nat_int nat off len
  then nth_digit_nat nat off
  else failwith "sys_int_of_nat"
;;

let int_of_nat nat =
  sys_int_of_nat nat 0 (length_nat nat)
;;

let nat_of_int i =
 if i < 0 then invalid_arg "nat_of_int" else
 let res = create_nat 1 in
 set_digit_nat res 0 i;
 res
;;

let eq_nat nat1 off1 len1 nat2 off2 len2 =
  compare_nat nat1 off1 (num_digits_nat nat1 off1 len1)
              nat2 off2 (num_digits_nat nat2 off2 len2) == 0
and le_nat nat1 off1 len1 nat2 off2 len2 =
  compare_nat nat1 off1 (num_digits_nat nat1 off1 len1)
              nat2 off2 (num_digits_nat nat2 off2 len2) <= 0
and lt_nat nat1 off1 len1 nat2 off2 len2 =
  compare_nat nat1 off1 (num_digits_nat nat1 off1 len1)
              nat2 off2 (num_digits_nat nat2 off2 len2) < 0
and ge_nat nat1 off1 len1 nat2 off2 len2 =
  compare_nat nat1 off1 (num_digits_nat nat1 off1 len1)
              nat2 off2 (num_digits_nat nat2 off2 len2) >= 0
and gt_nat nat1 off1 len1 nat2 off2 len2 =
  compare_nat nat1 off1 (num_digits_nat nat1 off1 len1)
              nat2 off2 (num_digits_nat nat2 off2 len2) > 0
;;

let square_nat nat1 off1 len1 nat2 off2 len2 =
  mult_nat nat1 off1 len1 nat2 off2 len2 nat2 off2 len2
;;

let set_square_nat nat1 off1 len1 nat2 off2 len2 =
  let _ = square_nat nat1 off1 len1 nat2 off2 len2 in ();;

let gcd_int_nat i nat off len = 
  if i == 0 then 1 else
  if is_nat_int nat off len then begin
    set_digit_nat nat off (gcd_int (nth_digit_nat nat off) i); 0
  end else begin
    let len_copy = succ len in
    let copy = create_nat len_copy 
    and quotient = create_nat 1 
    and remainder = create_nat 1 in
    blit_nat copy 0 nat off len;
    set_digit_nat copy len 0;
    div_digit_nat quotient 0 remainder 0 copy 0 len_copy (nat_of_int i) 0;
    set_digit_nat nat off (gcd_int (nth_digit_nat remainder 0) i);
    0
  end
;;

let exchange r1 r2 =
  let old1 = !r1 in r1 := !r2; r2 := old1
;;

let gcd_nat nat1 off1 len1 nat2 off2 len2 =
  if is_zero_nat nat1 off1 len1 then begin
    blit_nat nat1 off1 nat2 off2 len2; len2
  end else begin
    let copy1 = ref (create_nat (succ len1))
    and copy2 = ref (create_nat (succ len2)) in
      blit_nat !copy1 0 nat1 off1 len1;
      blit_nat !copy2 0 nat2 off2 len2;
      set_digit_nat !copy1 len1 0;
      set_digit_nat !copy2 len2 0;
      if lt_nat !copy1 0 len1 !copy2 0 len2
         then exchange copy1 copy2;
      let real_len1 = 
            ref (num_digits_nat !copy1 0 (length_nat !copy1))
      and real_len2 = 
            ref (num_digits_nat !copy2 0 (length_nat !copy2)) in
      while not (is_zero_nat !copy2 0 !real_len2) do
        set_digit_nat !copy1 !real_len1 0;
        div_nat !copy1 0 (succ !real_len1) !copy2 0 !real_len2;
        exchange copy1 copy2;
        real_len1 := !real_len2;
        real_len2 := num_digits_nat !copy2 0 !real_len2
      done;                
      blit_nat nat1 off1 !copy1 0 !real_len1;
      !real_len1
  end
;;


(* Racine carrée entière par la méthode de Newton (entière par défaut). *)

(* Théorème: la suite xn+1 = (xn + a/xn) / 2 converge vers la racine *)
(* carrée entière de a par défaut, si on part d'une valeur x0 *)
(* strictement plus grande que la racine de a, sauf quand a est un *)
(* carré - 1, cas auquel la suite alterne entre la racine par défaut *)
(* et par excès. Dans tous les cas, le dernier terme de la partie *)
(* strictement décroissante de la suite est le résultat cherché. *)

let sqrt_nat rad off len =
 let len = num_digits_nat rad off len in
 (* Copie de travail du radicande *)
 let len_parity = len mod 2 in
 let rad_len = len + 1 + len_parity in
 let rad =
   let res = create_nat rad_len in
   blit_nat res 0 rad off len;
   set_digit_nat res len 0;
   set_digit_nat res (rad_len - 1) 0;
   res in
 let cand_len = (len + 1) / 2 in  (* ceiling len / 2 *) 
 let cand_rest = rad_len - cand_len in
 (* Racine carrée supposée cand = "|FFFF .... |" *)
 let cand = make_nat cand_len in
 (* Amélioration de la racine de départ:
    on calcule nbb le nombre de bits significatifs du premier digit du candidat
    (la moitié du nombre de bits significatifs dans les deux premiers
     digits du radicande étendu à une longueur paire).
    shift_cand est word_size - nbb *)
 let shift_cand =
   ((num_leading_zero_bits_in_digit rad (len-1)) +
     sys__word_size * len_parity) / 2 in
 (* Tous les bits du radicande sont à 0, on rend 0. *)
 if shift_cand == sys__word_size then cand else
 begin
  complement_nat cand 0 cand_len;
  shift_right_nat cand 0 1 tmp_A_1 0 shift_cand;
  let next_cand = create_nat rad_len in
  (* Repeat until *)
  let rec loop () =
           (* next_cand := rad *)
   blit_nat next_cand 0 rad 0 rad_len;
           (* next_cand <- next_cand / cand *)
   div_nat next_cand 0 rad_len cand 0 cand_len;
           (* next_cand (poids fort) <- next_cand (poids fort) + cand,
              i.e. next_cand <- cand + rad / cand *)
   set_add_nat next_cand cand_len cand_rest cand 0 cand_len 0;
        (* next_cand <- next_cand / 2 *)
   shift_right_nat next_cand cand_len cand_rest tmp_A_1 0 1;
   if lt_nat next_cand cand_len cand_rest cand 0 cand_len then
    begin  (* cand <- next_cand *)
     blit_nat cand 0 next_cand cand_len cand_len; loop ()
    end
   else cand in
  loop ()
 end;;

let max_superscript_10_power_in_int =
 match sys__word_size with
 | 64 -> 18
 | 32 -> 9
 | _ -> invalid_arg "Bad word size";;

let max_power_10_power_in_int =
 match sys__word_size with
 | 64 -> nat_of_int 1000000000000000000
 | 32 -> nat_of_int 1000000000
 | _ -> invalid_arg "Bad word size";;

let sys_string_of_digit nat off =
  if is_nat_int nat off 1 then string_of_int (nth_digit_nat nat off) else
   begin
    blit_nat tmp_B_2 0 nat off 1;
    set_to_zero_nat tmp_B_2 1 1;
    div_digit_nat tmp_A_2 0 tmp_A_1 0 tmp_B_2 0 2 max_power_10_power_in_int 0;
    let leading_digits = nth_digit_nat tmp_A_2 0
    and s1 = string_of_int (nth_digit_nat tmp_A_1 0) in
    let len = string_length s1 in
    if leading_digits < 10 then begin
         let result = make_string (max_superscript_10_power_in_int + 1) `0` in
         result.[0] <- char_of_int (48 + leading_digits);
         blit_string s1 0 result (string_length result - len) len;
         result
    end else begin
         let result = make_string (max_superscript_10_power_in_int + 2) `0` in
         blit_string (string_of_int leading_digits) 0 result 0 2;
         blit_string s1 0 result (string_length result - len) len;
         result
    end
   end
;;

let string_of_digit nat =
    sys_string_of_digit nat 0
;;

(* make_power_base affecte power_base des puissances successives de base a 
   partir de la puissance 1-ieme.
   A la fin de la boucle i-1 est la plus grande puissance de la base qui tient 
   sur un seul digit et j est la plus grande puissance de la base qui tient 
   sur un int.
   Attention base n''est pas forcément une base valide (utilisé en
   particulier dans big_int avec un entier quelconque). *)
let make_power_base base power_base = 
  let i = ref 1
  and j = ref 0 in
  set_digit_nat power_base 0 base;
  while is_digit_zero power_base !i do
   set_mult_digit_nat power_base !i 2 power_base (pred !i) 1 power_base 0;
   incr i
  done;
  decr i;
  while !j <= !i && is_digit_int power_base !j do incr j done;
  (!i - 1, min !i !j);;

(* On compte les zéros placés au début de la chaîne, 
   on en déduit la longueur réelle de la chaîne 
   et on construit la chaîne adhoc en y ajoutant before et after. *)
let adjust_string s before after =
  let len_s = string_length s
  and k = ref 0 in
    while !k < len_s - 1 && s.[!k] == `0`
       do incr k 
    done;
  let len_before = string_length before
  and len_after = string_length after 
  and l1 = max (len_s - !k) 1 in
  let l2 = len_before + l1 in
  if l2 <= 0 then failwith "adjust_string" else
  let ok_len = l2 + len_after in
  let ok_s = create_string ok_len in
  blit_string before 0 ok_s 0 len_before;
  blit_string s !k ok_s len_before l1;
  blit_string after 0 ok_s l2 len_after;
  ok_s
;;

let power_base_int base i =
  if i == 0 then
    nat_of_int 1 
  else if i < 0 then
    invalid_arg "power_base_int"
  else begin
         let power_base = make_nat (succ length_of_digit) in
         let (pmax, pint) = make_power_base base power_base in
         let n = i / (succ pmax) 
         and rem = i mod (succ pmax) in
           if n > 0 then begin
               let newn =
                 if i == biggest_int then n else (succ n) in
               let res = make_nat newn
               and res2 = make_nat newn
               and l = num_bits_int n - 2 in
               let p = ref (1 lsl l) in
                 blit_nat res 0 power_base pmax 1;
                 for i = l downto 0 do
                   let len = num_digits_nat res 0 newn in
                   let len2 = min n (2 * len) in
                   let succ_len2 = succ len2 in
                     set_square_nat res2 0 len2 res 0 len;
                     if n land !p > 0 then begin
                       set_to_zero_nat res 0 len;
                       set_mult_digit_nat res 0 succ_len2 
                                      res2 0 len2 
                                      power_base pmax
                     end else
                       blit_nat res 0 res2 0 len2;
                     set_to_zero_nat res2 0 len2;
                     p := !p lsr 1
                 done;
               if rem > 0 then begin
                 set_mult_digit_nat res2 0 newn 
                                res 0 n power_base (pred rem);
                 res2
               end else res
            end else 
              copy_nat power_base (pred rem) 1
  end
;;

(* PW: rajoute avec 32 et 64 bits *)

(* the base-th element (base >= 2) of num_digits_max_vector is :
    |                                     |
    | sys__max_string_length * log (base) |
    | ----------------------------------- | + 1
    |      length_of_digit * log (2)      |
    --                                   --
La base la plus grande possible pour l'impression est 16. *)

(* num_digits_max_vector.(base) gives the maximum number of words that
   may have the biggest big number that can be printed into a single
   character string of maximum length (the number being printed in
   base [base]). (This computation takes into account the size of the
   machine word (length_of_digit or size_word).) *)

let num_digits_max_vector =
 match sys__word_size with
 | 64 ->
    [| 0; 0; 262143; 415488; 524287; 608679; 677632; 735930; 786431; 830976; 
       870823; 906868; 939776; 970047; 998074; 1024167; 1048575; |]
 | 32 ->
    [| 0; 0; 524287; 830976; 1048575; 1217358; 1355264; 1471861; 1572863; 
       1661953; 1741646; 1813737; 1879552; 1940095; 1996149; 2048335;
       2097151 |]
 | _ -> invalid_arg "Bad word size";;

let zero_nat = make_nat 1;;

let power_max_map = make_vect 17 zero_nat;;

let power_max base =
 let v = power_max_map.(base) in
 if v != zero_nat then v else begin
   let v = power_base_int base sys__max_string_length in
   power_max_map.(base) <- v; v
  end
;;

let sys_string_list_of_nat base nat off len =
  if is_nat_int nat off len 
  then [sys_string_of_int base "" (nth_digit_nat nat off) ""] else begin
  (* pmax : L'indice de la plus grande puissance de base qui soit un digit 
     pint : La plus grande puissance de base qui soit un int 
     power_base : nat de (length_of_digit + 1) digits dont le i-ème digit 
     contient base^(i+1) *)
  check_base base;
  let power_base = make_nat (succ length_of_digit) in
  let (pmax, pint) = make_power_base base power_base in
  (* La représentation de 2^length_of_digit en base base
     a real_pmax chiffres *)
  let real_pmax = pmax + 2
  and num_int_in_digit = pmax / pint
  (* new_nat est une copie de nat qui a un chiffre de plus à cause de
     la division *)
  and new_nat = make_nat (succ len)
  and len_copy = ref (succ (num_digits_nat nat off len)) in
  let len_new_nat = ref !len_copy in 
  (* copy1 et copy2 sont en fait 2 noms pour un même contenu, 
     copy1 est l'argument sur lequel se fait la division, 
     copy2 est le quotient de la division 
     et on remet copy1 à la bonne valeur à la fin de la boucle *)
  let copy1 = create_nat !len_copy
  and copy2 = make_nat !len_copy
  and rest_digit = make_nat 2
  and rest_int = create_nat 1
  and places = ref 0 in
  (* On divise nat par power_max jusqu'à épuisement, on écrit les restes 
     successifs, la représentation de ces nombres en base base tient sur 
     biggest_int chiffres donc sur une chaîne de caractères *)
  let length_block = num_digits_max_vector.(base) in
  let l = ref ([] : string list) in
    len_copy := pred !len_copy; 
    blit_nat new_nat 0 nat 0 len;
    while not (is_zero_nat new_nat 0 !len_new_nat) do
       let len_s = 
         if !len_new_nat <= length_block
            then let cand = real_pmax * !len_new_nat in
                   (if cand <= 0 then sys__max_string_length else cand)
            else sys__max_string_length in
       (if !len_new_nat > length_block
          then (let power_max_base = power_max base in
                div_nat new_nat 0 !len_new_nat power_max_base 0 length_block; 
                blit_nat copy1 0 new_nat 0 length_block;
                len_copy := num_digits_nat copy1 0 length_block;
                len_new_nat := max 1 (!len_new_nat - length_block);
                blit_nat new_nat 0 new_nat length_block !len_new_nat;
                set_to_zero_nat new_nat !len_new_nat length_block;
                (* new_nat a un premier digit nul pour les divisions 
                   ultérieures éventuelles *)
                len_new_nat := 
                  (if is_zero_nat new_nat 0 !len_new_nat 
                     then 1 
                     else succ (
                            num_digits_nat new_nat 0 !len_new_nat)))
          else (blit_nat copy1 0 new_nat 0 !len_new_nat;
                len_copy := num_digits_nat copy1 0 !len_new_nat;
                set_to_zero_nat new_nat 0 !len_new_nat;
                len_new_nat := 1));
      let s = make_string len_s `0`
      and pos_ref = ref (pred len_s) in
        while not (is_zero_nat copy1 0 !len_copy) do 
           (* On récupère un digit dans rest_digit *)
           set_digit_nat copy1 !len_copy 0; 
           div_digit_nat copy2 0
                         rest_digit 0 copy1 0 (succ !len_copy) power_base pmax;
           places := succ pmax;
           for j = 0 to num_int_in_digit do
             (* On récupère un int dans rest_int.
                La valeur significative de copy se trouve dans copy2 
                on peut donc utiliser copy1 pour stocker la valeur du 
                quotient avant de la remettre dans rest_digit. *)
             if compare_digits_nat rest_digit 0 power_base (pred pint) == 0
                then (set_digit_nat rest_digit 0 1;
                      set_digit_nat rest_int 0 0)
                else (div_digit_nat copy1 0 rest_int 0 rest_digit 0 2
                                     power_base (pred pint);
                      blit_nat rest_digit 0 copy1 0 1);
             (* On l'écrit dans la chaîne s en lui réservant la place 
                nécessaire. *)
             int_to_string 
               (nth_digit_nat rest_int 0) s pos_ref base 
                 (if is_zero_nat copy2 0 !len_copy then min !pos_ref pint else
                  if !places > pint then (places := !places - pint; pint)
                  else !places)
           done;
         len_copy := num_digits_nat copy2 0 !len_copy; 
         blit_nat copy1 0 copy2 0 !len_copy
      done;
      if is_zero_nat new_nat 0 !len_new_nat
          then l := adjust_string s "" "" :: !l
          else l := s :: !l
    done; !l
  end
;;

(* Power_base_max is used *)
let power_base_max = make_nat 2;;

let pmax =
 match sys__word_size with
 | 64 ->
    set_digit_nat power_base_max 0 1000000000000000000;
    set_mult_digit_nat power_base_max 0 2 
                   power_base_max 0 1 (nat_of_int 9) 0;
    19
 | 32 ->
    set_digit_nat power_base_max 0 1000000000;
    9
 | _ -> invalid_arg "Bad word size";;

let unadjusted_string_of_nat nat off len_nat =
  let len = num_digits_nat nat off len_nat in
  if len == 1 then sys_string_of_digit nat off else
  let len_copy = ref (succ len) in
  let copy1 = create_nat !len_copy
  and copy2 = make_nat !len_copy
  and rest_digit = make_nat 2 in
    if len > biggest_int / (succ pmax)
       then failwith "number too long" 
       else let len_s = (succ pmax) * len in
            let s = make_string len_s `0`
            and pos_ref = ref len_s in
              len_copy := pred !len_copy; 
              blit_nat copy1 0 nat off len;
              set_digit_nat copy1 len 0;
              while not (is_zero_nat copy1 0 !len_copy) do  
                 div_digit_nat copy2 0 
                                rest_digit 0 
                                copy1 0 (succ !len_copy) 
                                power_base_max 0;
                 let str = sys_string_of_digit rest_digit 0 in
                 blit_string str 0 
                             s (!pos_ref - string_length str)
                             (string_length str);
                 pos_ref := !pos_ref - pmax;
                 len_copy := num_digits_nat copy2 0 !len_copy; 
                 blit_nat copy1 0 copy2 0 !len_copy;
                 set_digit_nat copy1 !len_copy 0 
              done;
              s
;;

let string_of_nat nat = 
 let s = unadjusted_string_of_nat nat 0 (length_nat nat) 
 and index = ref 0 in
 begin try
  for i = 0 to string_length s - 2 do
   if s.[i] != `0` then (index := i; raise Exit)
  done
 with Exit -> () end;
 sub_string s !index (string_length s - !index)
;;

let sys_string_of_nat base before nat off len after =
  if base == 10 then 
    if num_digits_nat nat off len == 1 &&
       string_length before == 0 && 
       string_length after == 0
    then sys_string_of_digit nat off
    else adjust_string
          (unadjusted_string_of_nat nat off len) before after else
  if is_nat_int nat off len
  then sys_string_of_int base before (nth_digit_nat nat off) after
  else let sl = sys_string_list_of_nat base nat off len in
       match sl with
       | [s] -> adjust_string s before after
       | _ -> invalid_arg "sys_string_of_nat"
;;


(* Pour debugger, on écrit les digits du nombre en base 16, séparés par
   des barres: |dn|dn-1 ...|d0| *)
let power_debug = nat_of_int 256;;

(* Nombre de caractères d'un digit écrit en base 16, avec un caractère
   supplémentaire pour la barre de séparation des digits. *)
let chars_in_digit_debug = succ (length_of_digit / 4);;

let debug_string_vect_nat nat =
  let len_n = length_nat nat in
  let max_digits = sys__max_string_length / chars_in_digit_debug in
  let blocks = len_n / max_digits
  and rest = len_n mod max_digits in
  let length = chars_in_digit_debug * max_digits
  and vs = make_vect (succ blocks) "" in
    for i = 0 to blocks do
      let len_s =
       if i == blocks
        then 1 + chars_in_digit_debug * rest else length in
      let s = make_string len_s `0` 
      and pos = ref (len_s - 1) in
      let treat_int int end_digit =
        decr pos;
        s.[!pos] <- digits.[int mod 16];
        let rest_int = int asr 4 in
        decr pos;
        s.[!pos] <- digits.[rest_int mod 16];
        if end_digit then (decr pos; s.[!pos] <- `|`)
      in s.[!pos] <- `|`;
         for j = i * max_digits to pred (min len_n (succ i * max_digits)) do
             let digit = make_nat 1
             and digit1 = make_nat 2
             and digit2 = make_nat 2 in
               blit_nat digit1 0 nat j 1;
               for k = 1 to pred (length_of_digit / 8) do
                   div_digit_nat digit2 0 digit 0 digit1 0 2 power_debug 0;
                   blit_nat digit1 0 digit2 0 1;
                   treat_int (nth_digit_nat digit 0) false
               done;
               treat_int (nth_digit_nat digit1 0) true
         done;
     vs.(i) <- s
     done;
     vs
;;

let debug_string_nat nat =
  let vs = debug_string_vect_nat nat in
  if vect_length vs == 1 then vs.(0) else invalid_arg "debug_string_nat"
;;

(* La sous-chaine (s, off, len) représente un nat en base base que 
   on détermine ici. *)
let simple_sys_nat_of_string base s off len = 
  (* check_base base; : inutile la base est vérifiée par base_digit_of_char *)
  let power_base = make_nat (succ length_of_digit) in
  let (pmax, pint) = make_power_base base power_base in
  let new_len = ref (1 + len / (pmax + 1))
  and current_len = ref 1 in
  let possible_len = ref (min 2 !new_len) in

  let nat1 = make_nat !new_len
  and nat2 = make_nat !new_len 

  and digits_read = ref 0 
  and bound = off + len - 1
  and int = ref 0 in

  for i = off to bound do
    (* On lit pint (au maximum) chiffres, on en fait un int 
       et on l'intègre au nombre. *)
      let c = s.[i]  in
        begin match c with 
        | ` ` | `\t` | `\n` | `\r` | `\\` -> ()
        | _ -> int := !int * base + base_digit_of_char base c;
               incr digits_read
        end;
        if (!digits_read == pint || i == bound) && not (!digits_read == 0)
        then 
          begin
           set_digit_nat nat1 0 !int;
           let erase_len = if !new_len = !current_len then !current_len - 1
                           else !current_len in
           for j = 1 to erase_len do 
             set_digit_nat nat1 j 0
           done;
           set_mult_digit_nat nat1 0 !possible_len 
                          nat2 0 !current_len 
                          power_base (pred !digits_read);
           blit_nat nat2 0 nat1 0 !possible_len;
           current_len := num_digits_nat nat1 0 !possible_len;
           possible_len := min !new_len (succ !current_len);
           int := 0;
           digits_read := 0
           end
  done;
  (* On recadre le nat *)
  let nat = create_nat !current_len in
    blit_nat nat 0 nat1 0 !current_len;
    nat
;;

(* base_power_nat base n nat compute nat*base^n *)
let base_power_nat base n nat =
  match sign_int n with
  | 0 -> nat
  | -1 -> let base_nat = power_base_int base (- n) in
          let len_base_nat = num_digits_nat base_nat 0 (length_nat base_nat) 
          and len_nat = num_digits_nat nat 0 (length_nat nat) in
          if len_nat < len_base_nat then invalid_arg "base_power_nat" else
          if len_nat == len_base_nat &&
             compare_digits_nat nat len_nat base_nat len_base_nat == -1
          then invalid_arg "base_power_nat"
          else
           let copy = create_nat (succ len_nat) in
           blit_nat copy 0 nat 0 len_nat;
           set_digit_nat copy len_nat 0;
           div_nat copy 0 (succ len_nat) base_nat 0 len_base_nat;
           if not (is_zero_nat copy 0 len_base_nat)
           then invalid_arg "base_power_nat"
           else copy_nat copy len_base_nat 1
  | _ -> let base_nat = power_base_int base n in
         let len_base_nat = num_digits_nat base_nat 0 (length_nat base_nat) 
         and len_nat = num_digits_nat nat 0 (length_nat nat) in
         let new_len = len_nat + len_base_nat in
         let res = make_nat new_len in
          if len_nat > len_base_nat
             then set_mult_nat res 0 new_len 
                           nat 0 len_nat 
                           base_nat 0 len_base_nat
             else set_mult_nat res 0 new_len 
                           base_nat 0 len_base_nat 
                           nat 0 len_nat;
         if is_zero_nat res 0 new_len then zero_nat
         else res
;;

(* Tests if s has only zeros characters from index i to index lim *)
let rec only_zeros s i lim =
 i >= lim || s.[i] == `0` && only_zeros s (succ i) lim;;

(* Parses a string d*.d*e[+/-]d* *)
let rec only_zeros s i lim =
 i >= lim || s.[i] == `0` && only_zeros s (succ i) lim;;

(* Parses a string d*.d*e[+/-]d* *)
let decimal_of_string base s off len =
 (* Skipping leading + sign if any *)
 let skip_first = s.[off] == `+` in
 let offset = if skip_first then off + 1 else off
 and length = if skip_first then len - 1 else len in

 let offset_limit = offset + length - 1 in

 try
  let dot_pos = index_char_from s offset `.` in
  try
   if dot_pos = offset_limit then raise Not_found else
   let e_pos = index_char_from s (dot_pos + 1) `e` in
   (* int.int e int *)
   let e_arg =
      if e_pos = offset_limit then 0 else
     sys_int_of_string base s (succ e_pos) (offset_limit - e_pos) in
   let exponant = e_arg - (e_pos - dot_pos - 1) in
   let s_res = create_string (e_pos - offset - 1) in
   let int_part_length = dot_pos - offset in
   blit_string s offset s_res 0 int_part_length;
   blit_string s (dot_pos + 1) s_res int_part_length (e_pos - dot_pos - 1);
   s_res, exponant
  with Not_found ->
   (* `.` found, no `e` *)
   if only_zeros s (dot_pos + 1) (offset_limit + 1)
   then (sub_string s offset (dot_pos - offset), 0)
   else
    let exponant = - (offset_limit - dot_pos) in
    let s_res = create_string (length - 1) in
    let int_part_length = dot_pos - offset in
    blit_string s offset s_res 0 int_part_length;
    if dot_pos < offset_limit then
      blit_string s (dot_pos + 1)
                  s_res int_part_length (offset_limit - dot_pos);
    (s_res, exponant)
 with Not_found ->
   (* no `.` *)
   try
    (* int e int *)
    let e_pos = index_char_from s offset `e` in
    let e_arg =
      if e_pos = offset_limit then 0 else
      sys_int_of_string base s (succ e_pos) (offset_limit - e_pos) in
    let exponant = e_arg in
    let int_part_length = e_pos - offset in
    let s_res = create_string int_part_length in
    blit_string s offset s_res 0 int_part_length;
    s_res, exponant
   with Not_found ->
   (* a bare int *)
   (sub_string s offset length, 0);;


(* La chaîne s contient un entier en notation scientifique, de off sur
   une longueur de len *)
let sys_nat_of_string base s off len = 
  let (snat, k) = decimal_of_string base s off len in
  let len_snat = string_length snat in
  if k < 0 then begin
    for i = len_snat + k to pred len_snat do
     if snat.[i] != `0` then failwith "sys_nat_of_string"
    done;
    simple_sys_nat_of_string base snat 0 (len_snat + k)
   end
  else base_power_nat base k (simple_sys_nat_of_string base snat 0 len_snat)
;;

let nat_of_string s = sys_nat_of_string 10 s 0 (string_length s);;

let sys_float_of_nat nat off len = 
 float_of_string (sys_string_of_nat 10 "" nat off len ".0");;

let float_of_nat nat = sys_float_of_nat nat 0 (length_nat nat);;

let nat_of_float f = nat_of_string (string_of_float f);;

(* Nat printing *)

#open "format";;

let string_for_read_of_nat n =
    sys_string_of_nat 10 "#<" n 0 (length_nat n) ">";;

let sys_print_nat base before nat off len after =
  print_string before;
  do_list print_string (sys_string_list_of_nat base nat off len);
  print_string after
;;

let print_nat nat =
  sys_print_nat 10 "" nat 0 (num_digits_nat nat 0 (length_nat nat)) ""
;;

let print_nat_for_read nat = 
  sys_print_nat 10 "#<" nat 0 (num_digits_nat nat 0 (length_nat nat)) ">"
;;

let debug_print_nat nat =
  let vs = debug_string_vect_nat nat in
    for i = pred (vect_length vs) downto 0 do
      print_string vs.(i) 
    done
;;

