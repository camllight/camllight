#open "graphics";;

open_graph "";;

let round x =
  if x >=. 0.0
  then int_of_float (x +. 0.5)
  else - (int_of_float (-. x +. 0.5));;

type état =
   { mutable X : float; mutable Y : float;
     mutable Visée : float; mutable Levé : bool };;

let crayon = { X = 0.0; Y = 0.0; Visée = 0.0; Levé = false };;

let fixe_crayon b = crayon.Levé <- b;;

let pi_sur_180 =
    let pi = 4.0 *. (atan 1.0) in pi /. 180.0;;

let tourne angle =
    crayon.Visée <- (crayon.Visée +. angle *. pi_sur_180);;

let avance d =
    let dx = d *. cos (crayon.Visée)
    and dy = d *. sin (crayon.Visée) in
    crayon.X <- crayon.X +. dx;
    crayon.Y <- crayon.Y +. dy;
    if crayon.Levé
    then moveto (round crayon.X) (round crayon.Y)
    else lineto (round crayon.X) (round crayon.Y);;

let couleur_du_tracé = foreground;;
let couleur_du_fond = background;;
let zéro_x = float_of_int ((size_x ()) / 2);;
let zéro_y = float_of_int ((size_y ()) / 2);;
let vide_écran () =
    clear_graph();
    set_color couleur_du_tracé;
    crayon.X <- zéro_x;
    crayon.Y <- zéro_y;
    crayon.Visée <- 0.0;
    crayon.Levé <- false;
    moveto (round crayon.X) (round crayon.Y);;
