#open "graphics";;

type graphic_context =
  { array: int vect;                    (* Data to sort *)
    x0: int;                            (* X coordinate, lower left corner *)
    y0: int;                            (* Y coordinate, lower left corner *)
    width: int;                         (* Width in pixels *)
    height: int;                        (* Height in pixels *)
    nelts: int;                         (* Number of elements in the array *)
    maxval: int;                        (* Max value in the array + 1 *)
    rad: int;                           (* Dimension of the rectangles *)
    mutable action: status              (* What to do next *)
  }
;;

let background = white
and foreground = black;;

let draw gc i v =
  fill_rect (gc.x0 + (gc.w * i) / gc.nelts) (gc.y0 + (gc.h * v) / gc.maxval)
            gc.rad gc.rad
;;

let assign gc i v =
  set_color background; draw gc i gc.array.(i);
  set_color foreground; draw gc i v;
  gc.array.(i) <- v
;;

let exchange gc i j =
  let val_i = gc.array.(i) in
  assign gc i gc.array.(j);
  assign gc j val_i
;;

let initialize name funct array maxval x y w h =
  let (_, label_height) = text_size name in
  let rad = (w - 2) / (vect_length array) - 1 in
  let gc =
    { array = copy_vect array;
      x0 = x + 1;                       (* Leave one pixel left for Y axis *)
      y0 = y + 1;                       (* Leave one pixel below for X axis *)
      width = w - 2;                    (* 1 pixel left, 1 pixel right *)
      height = h - 1 - label_height - rad;
      nelts = vect_length array;
      maxval = maxval;
      rad = rad;
      action = Finished } in
  moveto (gc.x0 - 1) (gc.y0 + gc.height);
  lineto (gc.x0 - 1) (gc.y0 - 1);
  lineto (gc.x0 + gc.width) (gc.y0 - 1);
  moveto (gc.x0 - 1) (gc.y0 + gc.height);
  draw_string name;
  for i = 0 to vect_length array - 1 do
    draw gc i array.(i)
  done;
  gc.action <- funct gc;
  gc
;;

let display functs nelts maxval =
  let seed = ref 0 in
  moveto 0 0; draw_string "Press a key to start...";
  while not (key_pressed()) do incr seed done;
  read_key();
  random__seed !seed;
  clear_screen();
  let a = make_vect nelts 0 in
  for i = 0 to nelts - 1 do
    a.(i) <- random__int maxval
  done;
  let q = queue__new () in
  for i = 0 to vect_length functs - 1 do
    let (name, funct, x, y, w, h) = functs.(i) in
    queue__add q (initialize name funct array maxval x y w h)
  done;
  let delay = ref 0 in
  try
    while not queue__empty q do
      let gc = queue__take q in
        begin match gc.action with
          Finished -> ()
        | Pause f -> gc.action <- f gc; queue__add q gc
        end;
      if key_pressed() then begin
        match read_key() with
          `q`|`Q` ->
            raise Exit
        | `0`...`9` as c ->
            delay := (int_of_char c - 48) * 500
      end;
      for i = 0 to !delay do () done
    done
  with Exit -> ()
;;
