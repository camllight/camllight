#open "syntaxe";;

type 'a env =
  { vars: (string * 'a) list;
    procs: (string * décl_proc) list;
    foncs: (string * décl_fonc) list };;

let environnement_initial p f =
  { vars=[]; procs=p; foncs=f };;

let ajoute_variable nom info env =
  { vars=(nom,info)::env.vars; procs=env.procs; foncs=env.foncs };;

let cherche nom liste =
  try assoc nom liste with Not_found -> raise(Pas_trouvé nom);;

let cherche_variable nom env = cherche nom env.vars
and cherche_fonction nom env = cherche nom env.foncs
and cherche_procédure nom env = cherche nom env.procs;;
