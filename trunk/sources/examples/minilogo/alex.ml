let rec saute_blancs flux =
  match flux with
  | [< ' ` ` >] -> saute_blancs flux  (* ` ` est l'espace *)
  | [< ' `\t` >] -> saute_blancs flux (* `\t` est la tabulation *)
  | [< ' `\n` >] -> saute_blancs flux (* `\n` est la fin de ligne *)
  | [< >] -> ();;

let rec saute_blancs flux =
  match flux with
  | [< ' (` ` | `\t` | `\n`) >] -> saute_blancs flux
  | [< >] -> ();;

let rec lire_entier accumulateur flux =
  match flux with
  | [< '(`0`..`9` as c) >] ->
      lire_entier (10 * accumulateur + int_of_char c - 48) flux
  | [< >] -> accumulateur;;

let rec lire_d�cimales accumulateur �chelle flux =
  match flux with
  | [< '(`0`..`9` as c) >] ->
      lire_d�cimales
        (accumulateur +.
           float_of_int(int_of_char c - 48) *. �chelle)
        (�chelle /. 10.0) flux
  | [< >] -> accumulateur;;

let tampon = "----------------";;

let rec lire_mot position flux =
  match flux with
  | [< '(`A`..`Z` | `a`..`z` | `�` | `�` | `_` as c) >] ->
      if position < string_length tampon then
        set_nth_char tampon position c;
      lire_mot (position+1) flux
  | [< >] ->
      sub_string tampon 0 (min position (string_length tampon));;

let rec lire_lex�me flux =
  saute_blancs flux;
  match flux with
  | [< '(`A`..`Z` | `a`..`z` | `�` | `�` as c) >] ->
      set_nth_char tampon 0 c;
      Mot(lire_mot 1 flux)
  | [< '(`0`..`9` as c) >] ->
      let n = lire_entier (int_of_char c - 48) flux in
      begin match flux with
      | [< '`.` >] ->
          Constante_flottante
            (lire_d�cimales (float_of_int n) 0.1 flux)
      | [< >] -> Constante_enti�re(n)
      end
  | [< 'c >] -> Symbole c;;

let analyseur_lexical flux =
  stream_from (fun () -> lire_lex�me flux);;
