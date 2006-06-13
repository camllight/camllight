(* This variant of spir.ml works better on black-and-white screens,
   and probably also on grayscale screens. *)

#open "graphics";;

(* Simple turtle graphics *)

type turtle_state =
  { mutable x : float;
    mutable y : float;
    mutable heading : float };;

let t = { x = 0.0; y = 0.0; heading = 0.0 };;

let pi180 = 4.0 *. atan 1.0 /. 180.0;;

let round x =
  if x >=. 0.0 then int_of_float(x +. 0.5) else -(int_of_float(0.5 -. x));;

let reset() =
  t.x <- float_of_int(size_x() / 2);
  t.y <- float_of_int(size_y() / 2);
  t.heading <- 0.0;
  moveto (round t.x) (round t.y)
;;

let forward d =
  t.x <- t.x +. cos(t.heading) *. d;
  t.y <- t.y +. sin(t.heading) *. d;
  lineto (round t.x) (round t.y)
;;

let turn a =
  t.heading <- t.heading +. a *. pi180
;;

(* The main drawing function *)

let rec spir dist angle angle_incr =
  if key_pressed() then () else begin
    forward dist;
    turn angle;
    spir dist (angle +. angle_incr) angle_incr
  end
;;
  
(* The interaction loop *)

let message s =
  let (x, y) = current_point() in
  draw_string s;
  let (_, height) = text_size s in
  moveto x (y + height)
;;

let format f =
  format_float "%6.2f" f
;;

let rec loop dist angle_incr =
  clear_graph();
  set_color foreground;
  moveto 0 0;
  message "   -           d, D             to decrease";
  message "   +           i, I             to increase";
  message (format dist ^ "      " ^ format angle_incr);
  message "Distance    Angle increment     'q' to quit";
  reset();
  spir dist 0.0 angle_incr;
  match read_key() with
  | `-` -> loop (dist -. 2.0) angle_incr
  | `+` -> loop (dist +. 2.0) angle_incr
  | `d` -> loop dist (angle_incr -. 0.05)
  | `D` -> loop dist (angle_incr -. 5.0)
  | `i` -> loop dist (angle_incr +. 0.05)
  | `I` -> loop dist (angle_incr +. 5.0)
  | `q` -> ()
  | _ -> loop dist angle_incr
;;

let spir () =
  open_graph "";
  loop 5.0 1.9;
  close_graph()
;;

if sys__interactive then () else begin spir(); exit 0 end;;
