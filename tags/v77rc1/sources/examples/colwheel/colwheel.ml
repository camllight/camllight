#open "graphics";;

(* Conversion from HSB (hue-saturation-brightness) to RGB.
   H, S and B are in the range 0..255. *)

let max = 255;;

let nround x y = (2*x+y)/(2*y);;

let rgb_of_hsb h s v =
  let h = h*6 in
  let i = h/max*max in
  let f = h-i in
  let m = v*(max-s)/max and n = v*(max-s*f/max)/max
  and k = v*(max-s*(max-f)/max)/max in
  graphics__rgb
    (nround (max*(
      match i/max with
      | 0 | 6 -> v | 1 -> n | 2 -> m | 3 -> m | 4 -> k | _ -> v
    )) max)
    (nround (max*(
      match i/max with
      | 0 | 6 -> k | 1 -> v | 2 -> v | 3 -> n | 4 -> m | _ -> m
    )) max)
    (nround (max*(
      match i/max with
      | 0 | 6 -> m | 1 -> m | 2 -> k | 3 -> v | 4 -> v | _ -> n
    )) max)
;;

let pi180 = 3.141592654 /. 180.0;;

let wheel s v r =
  for theta = 0 to 23 do
    set_color (rgb_of_hsb (theta * max / 24) s v);
    fill_arc (size_x()/2) (size_y()/2) r r (theta * 15) (theta * 15 + 15)
  done
;;

let wheels v =
  for r = 8 downto 1 do
    wheel (r * max / 8) v (r * (size_y()/20))
  done
;;

let main() =
  open_graph "";
  let (msg_w, msg_h) = text_size "Press 'q' to quit    R=999 G=999 B=999" in
  try
    wheels max;
    set_color foreground;
    moveto 0 0; draw_string "Press 'q' to quit";
    while true do
      let e = wait_next_event [Button_down; Key_pressed] in
        if e.keypressed then begin
          match e.key with
          | `0` .. `9` ->
              clear_graph();
              wheels ((int_of_char e.key - 48) * max / 9)
          | `q` | `Q` ->
              raise Exit
          | _ ->
              ()
        end else
        if e.button then begin
          let c = point_color e.mouse_x e.mouse_y in
          let r = c lsr 16 and g = (c lsr 8) land 255 and b = c land 255 in
            set_color background;
            fill_rect 0 0 msg_w msg_h;
            set_color foreground;
            moveto 0 0;
            draw_string ("Press 'q' to quit    R=" ^ string_of_int r ^
                         " G=" ^ string_of_int g ^ " B=" ^ string_of_int b)
        end
    done
  with Exit ->
    close_graph()
;;

if sys__interactive then () else begin main(); exit 0 end;;
