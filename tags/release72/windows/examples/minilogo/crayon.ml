#open "graphics";;

open_graph "";;

let round x =
  if x >=. 0.0
  then int_of_float (x +. 0.5)
  else - (int_of_float (-. x +. 0.5));;

type �tat =
   { mutable X : float; mutable Y : float;
     mutable Vis�e : float; mutable Lev� : bool };;

let crayon = { X = 0.0; Y = 0.0; Vis�e = 0.0; Lev� = false };;

let fixe_crayon b = crayon.Lev� <- b;;

let pi_sur_180 =
    let pi = 4.0 *. (atan 1.0) in pi /. 180.0;;

let tourne angle =
    crayon.Vis�e <- (crayon.Vis�e +. angle *. pi_sur_180);;

let avance d =
    let dx = d *. cos (crayon.Vis�e)
    and dy = d *. sin (crayon.Vis�e) in
    crayon.X <- crayon.X +. dx;
    crayon.Y <- crayon.Y +. dy;
    if crayon.Lev�
    then moveto (round crayon.X) (round crayon.Y)
    else lineto (round crayon.X) (round crayon.Y);;

let couleur_du_trac� = foreground;;
let couleur_du_fond = background;;
let z�ro_x = float_of_int ((size_x ()) / 2);;
let z�ro_y = float_of_int ((size_y ()) / 2);;
let vide_�cran () =
    clear_graph();
    set_color couleur_du_trac�;
    crayon.X <- z�ro_x;
    crayon.Y <- z�ro_y;
    crayon.Vis�e <- 0.0;
    crayon.Lev� <- false;
    moveto (round crayon.X) (round crayon.Y);;
