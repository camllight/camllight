(* Some extra operations on integers *)

(**) #open "eq";;
(**) #open "int";;

let length_of_int = sys__word_size - 2;;

let monster_int = 1 lsl length_of_int;;
let biggest_int = monster_int - 1;;
let least_int = - biggest_int;;

let rec num_bits_int_aux n =
  if n == 0 then 0 else succ (num_bits_int_aux (n lsr 1));;

let num_bits_int n = num_bits_int_aux (abs n);;

let sign_int i = if i == 0 then 0 else if i > 0 then 1 else -1;;

let compare_int n1 n2 =
  if n1 == n2 then 0 else if n1 > n2 then 1 else -1;;

let rec gcd_int i1 i2 =
  if i2 == 0 then abs i1 else gcd_int i2 (i1 mod i2)
;;

(* 
   int_to_string place la représentation de l' 'entier int en base base 
   dans la chaine s en le rangeant de la fin indiquée par pos vers le 
   début, sur times places et affecte à pos sa nouvelle valeur. 
*)
let digits = "0123456789ABCDEF";;

(* Vérification: une base doit être entre 2 et 16 *)
let check_base base =
 if base < 2 || base > 16 then invalid_arg "invalide base";;

(* Vérification: sous-chaîne valide *)
let check_string fname s off len =
 if off + len > string_length s || off < 0 || len < 0
 then invalid_arg fname;;

let int_to_string int s pos_ref base times = 
  let i = ref int 
  and j = ref times in
     while ((!i != 0) || (!j != 0)) && (!pos_ref != -1) do
        s.[!pos_ref] <- digits.[!i mod base];
        decr pos_ref;
        decr j;
        i := !i / base
     done
;;

let catenate before s off len after =
  let size = string_length s in
  let len_before = string_length before
  and len_after = string_length after in
  let l1 = len_before + len in
  let ok_len = l1 + len_after in

  if ok_len = size then s else begin
  let ok_s = create_string ok_len in
  blit_string before 0 ok_s 0 len_before;
  blit_string s off ok_s len_before len;
  blit_string after 0 ok_s l1 len_after;
  ok_s end
;;

let i_size = length_of_int + 2;;
let i_string = create_string i_size;;

let sys_string_of_int base before i after =
  if base = 10 then 
   let i_string = string_of_int i in
  catenate before i_string 0 (string_length i_string) after else begin
  check_base base;
  (* Écriture de la valeur absolue de i dans une chaîne *)
  let n = abs i in
  (* Écriture dans cette chaîne à partir de la fin *)
  let rec write n p =
   i_string.[p] <- digits.[n mod base];
   let m = n quo base in
   if m = 0 then p else write (n quo base) (p - 1) in
  let lim = write n (i_size - 1) in
  let off =
   if i < 0 then (i_string.[lim - 1] <- `-`; lim - 1) else lim in
  (* Nombre de caractères réellement écrits *)
  let written = i_size - off in
  (* Calcul du résultat final comprenant before et after *)
  catenate before i_string off written after end;;

(* Calcul la valeur d' 'un caractère en base base *)
let base_digit_of_char base c =
 let c_val = int_of_char c in
 let d =
  if c_val <= int_of_char `9` then c_val - int_of_char `0`
  else c_val - int_of_char `A` + 10 in
 if d < 0 then failwith "invalid digit" else
 if d >= base then failwith "invalid digit" else
 d
;;

(* Lit un petit entier signé dans une chaîne *)
let sys_int_of_string base s off len =
 check_base base;
 check_string "sys_int_of_string" s off len;
 let res = ref 0 in
 let start =
  match s.[off] with
  | `+` | `-` -> off + 1
  | _ -> off in
 for i = start to off + len - 1 do
  res := !res * base + base_digit_of_char base s.[i]
 done;
 match s.[off] with
 | `-` -> - !res
 | _ -> !res;;

