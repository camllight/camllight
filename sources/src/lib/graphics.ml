#open "int";;
#open "fstring";;
#open "eq";;

let rgb r g b =
  r lsl 16 + g lsl 8 + b
;;

let black   = 0
and white   = 0xFFFFFF
and red     = 0xFF0000
and green   = 0x00FF00
and blue    = 0x0000FF
and yellow  = 0xFFFF00
and cyan    = 0x00FFFF
and magenta = 0xFF00FF
;;

let background = black
and foreground = white
;;

let draw_ellipse x y rx ry =
  draw_arc x y rx ry 0 360
;;

let draw_circle x y r =
  draw_arc x y r r 0 360
;;

let set_line_width _ = ()
;;

let set_font _ = ()
and set_text_size _ = ()
;;

let fill_ellipse x y rx ry =
  fill_arc x y rx ry 0 360
;;

let fill_circle x y r =
  fill_arc x y r r 0 360
;;

let transp = -1
;;

let get_image x y w h =
  let i = create_image w h in
    blit_image i x y; i
;;

let mouse_pos () =
  let e = wait_next_event [Poll] in (e.mouse_x, e.mouse_y)
;;

let button_down () =
  let e = wait_next_event [Poll] in e.button
;;

let read_key () =
  let e = wait_next_event [Key_pressed] in e.key
;;

let key_pressed () =
  let e = wait_next_event [Poll] in e.keypressed
;;
