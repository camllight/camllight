(* Machine-independent graphic primitives *)

exception Graphic_failure of string;;
        (* Raised by the functions below when they encounter an error. *)

(*** Initializations *)

value open_graph: string -> unit = 1 "gr_open_graph"
        (* Switch the screen to graphic mode.
           The string argument can be used to specify a graphic mode.
           If the empty string is given, a sensible default mode is
           selected. *)
  and close_graph: unit -> unit = 1 "gr_close_graph"
        (* Switch the screen back to text mode. *)
  and clear_graph : unit -> unit = 1 "gr_clear_graph"
        (* Erase the graphic window. *)
  and size_x : unit -> int = 1 "gr_size_x"
  and size_y : unit -> int = 1 "gr_size_y"
        (* Return the size of the graphic window. Coordinates of the screen
           pixels range over [0 .. size_x() - 1] and [0 .. size_y()-1].
           Drawings outside of this rectangle are clipped, without causing
           an error. The origin (0,0) is at the lower left corner.
           The coordinate system used in the same as in mathematics:
           [y=0] is the bottom line of the window;
           [y=size_y()-1] is the top line;
           angles are measured counterclockwise. *)
;;

(*** Colors *)

type color == int
        (* A color is specified by its R, G, B components. Each component
           is in the range [0..255]. The three components are packed in an int:
           0xRRGGBB, where RR are the two hexadecimal digits for the red
           component, and so on. *)
;;

value set_color : color -> unit = 1 "gr_set_color"
        (* Set the current drawing color *)
;;

value rgb: int -> int -> int -> int;;
        (* [rgb r g b] returns the integer encoding the color with red
           component [r], green component [g], and blue component [b].
           [r], [g] and [b] are in the range [0..255]. *)

value black : color
  and white : color
  and red : color
  and green : color
  and blue : color
  and yellow : color
  and cyan : color
  and magenta : color
;;
        (* Some predefined colors *)

value background: color
        (* The color of the screen background (black on the PC ports).
           [clear_graph] paints the screen with this color. *)
  and foreground: color
        (* The initial drawing color (white on the PC ports). *)
;;

(*** Point and line drawing *)

value plot : int -> int -> unit = 2 "gr_plot"
        (* Plot the given point with the current drawing color. *)
  and point_color : int -> int -> color = 2 "gr_point_color"
        (* Return the color of the given point. *)
  and moveto : int -> int -> unit = 2 "gr_moveto"
        (* Position the current point. *)
  and current_point : unit -> int * int = 1 "gr_current_point"
        (* Return the position of the current point. *)
  and lineto : int -> int -> unit = 2 "gr_lineto"
        (* Draw a line with endpoints the current point and the given point,
           and move the current point to the given point. *)
  and draw_arc : int -> int -> int -> int -> int -> int -> unit
  	       = 6 "gr_draw_arc"
        (* [draw_arc x y rx ry a1 a2] draws an elliptical arc with center
           [x,y], horizontal radius [rx], vertical radius [ry], from angle
           [a1] to angle [a2] (in degrees). *)
  and draw_ellipse : int -> int -> int -> int -> unit
        (* [draw_ellipse x y rx ry] draws an ellipse with center
           [x,y], horizontal radius [rx] and vertical radius [ry]. *)
  and draw_circle : int -> int -> int -> unit
        (* [draw_circle x y r] draws a circle with center [x,y] and
           radius [r]. *)
  and set_line_width : int -> unit
        (* Set the width of points and lines drawn with the functions above.
           Currently does nothing in the PC ports. *)
;;

(*** Text drawing *)

value draw_char : char -> unit = 1 "gr_draw_char"
  and draw_string : string -> unit = 1 "gr_draw_string"
        (* Draw a character or a character string with lower left corner
           at current position. After drawing, the current position is set
           to the lower right corner of the text drawn. *)
  and set_font : string -> unit
  and set_text_size : int -> unit
        (* Set the font and character size used for drawing text.
           The interpretation of the arguments to [set_font] and
           [set_text_size] is implementation-dependent. On the PC ports,
           these functions currently do nothing. *)
  and text_size : string -> int * int = 1 "gr_text_size"
        (* Return the dimensions of the given text, if it were drawn with
           the current font and size. *)
;;

(*** Filling *)

value fill_rect : int -> int -> int -> int -> unit = 4 "gr_fill_rect"
        (* [fill_rect x y w h] fills the rectangle with lower left corner
           at [x,y], width [w] and heigth [h], with the current color. *)
  and fill_poly : (int * int) vect -> unit = 1 "gr_fill_poly"
        (* Fill the given polygon with the current color. The vector
           contains the coordinates of the vertices of the polygon. *)
  and fill_arc : int -> int -> int -> int -> int -> int -> unit
  	       = 6 "gr_fill_arc"
        (* Fill an elliptical pieslice with the current color. The
           parameters are the same as for [draw_arc]. *)
  and fill_ellipse : int -> int -> int -> int -> unit
        (* Fill an ellipse with the current color. The
           parameters are the same as for [draw_ellipse]. *)
  and fill_circle : int -> int -> int -> unit
        (* Fill a circle with the current color. The
           parameters are the same as for [draw_circle]. *)
;;

(*** Images *)

type image;;
        (* The abstract type for images, in internal representation.
           Externally, images are represented as matrices of colors. *)

value transp : color;;
        (* In matrices of colors, this color represent a ` `transparent' '
           point: when drawing the corresponding image, all pixels on the
           screen corresponding to a transparent pixel in the image will
           not be modified, while other points will be set to the color
           of the corresponding point in the image. This allows superimposing
           an image over an existing background. *)

value make_image : color vect vect -> image = 1 "gr_make_image"
        (* Convert the given color matrix to an image.
           Each sub-vector represent one horizontal line. All sub-vectors
           must have the same length; otherwise, exception [Graphic_failure]
           is raised. *)
  and dump_image : image -> color vect vect = 1 "gr_dump_image"
        (* Convert an image to a color matrix. *)
  and draw_image : image -> int -> int -> unit = 3 "gr_draw_image"
        (* Draw the given image with lower left corner at the given point. *)
  and get_image : int -> int -> int -> int -> image
        (* Capture the contents of a rectangle on the screen as an image.
           The parameters are the same as for [fill_rectangle]. *)
  and create_image : int -> int -> image = 2 "gr_create_image"
  	(* [create_image w h] allocates a new image with width [w] and 
	   height [h], for use with [copy_image]. *)
  and blit_image : image -> int -> int -> unit = 3 "gr_blit_image"
  	(* [blit_image i x y] copies the contents of a rectangle on
	   the screen into image [i]. The rectangle has lower left
	   corner [(x,y)], and width and height those of image [i]. *)
;;

(*** Mouse and keyboard events *)

type status =
  { mouse_x : int;		(* X coordinate of the mouse *)
    mouse_y : int;		(* Y coordinate of the mouse *)
    button : bool;		(* true if a mouse button is pressed *)
    keypressed : bool;          (* true if a key has been pressed *)
    key : char }		(* the character for the key pressed *)
;;

type event =
    Button_down			(* A mouse button is pressed *)
  | Button_up			(* A mouse button is released *)
  | Key_pressed			(* A key is pressed *)
  | Mouse_motion		(* The mouse is moved *)
  | Poll			(* Don't wait; return immediately *)
;;

value wait_next_event : event list -> status = 1 "gr_wait_event"
;;

(*** Mouse and keyboard polling *)

value mouse_pos : unit -> int * int
        (* Return the position of the mouse cursor, relative to the
           graphic window. If the mouse cursor is outside of the graphic
           window, [mouse_pos()] returns a point outside of the range
           [0..size_x()-1, 0..size_y()-1]. *)
  and button_down : unit -> bool
        (* Return [true] if the mouse button is pressed, [false] otherwise. *)
  and read_key : unit -> char
        (* Wait for a key to be pressed (in the graphic window), and return
           the corresponding character. Keypresses are queued. *)
  and key_pressed : unit -> bool
        (* Return [true] if a keypress is available; that is, if [read_key]
           would not block. *)
;;

(*** Sound *)

value sound : int -> int -> unit = 2 "gr_sound"
        (* [sound freq dur] plays a sound at frequency [freq] (in hertz)
           for a duration [dur] (in milliseconds). *)
;;
