/* Graphics primitives */

#include "mlvalues.h"
#include "fail.h"

/////////////////////////////////////////////////////////////////////////////
// If HAS_UI is defined, graphics pimitives are defined elsewhere:
#ifndef HAS_UI 

value gr_open_graph(mode)	/* ML */
     value mode;
{
  invalid_argument("open_graph");
  return Val_unit;
}

value gr_close_graph()	/* ML */
{
  invalid_argument("close_graph");
  return Val_unit;
}

value gr_clear_graph()	/* ML */
{
  invalid_argument("clear_graph");
  return Val_unit;
}

value gr_size_x()	/* ML */
{
  invalid_argument("size_x");
  return Val_unit;
}

value gr_size_y()	/* ML */
{
  invalid_argument("size_y");
  return Val_unit;
}

value gr_set_color(color)	/* ML */
     value color;
{
  invalid_argument("set_color");
  return Val_unit;
}

value gr_plot(x, y)	/* ML */
     value x, y;
{
  invalid_argument("plot");
  return Val_unit;
}

value gr_point_color(x, y)	/* ML */
     value x, y;
{
  invalid_argument("point_color");
  return Val_unit;
}

value gr_moveto(x, y)	/* ML */
     value x, y;
{
  invalid_argument("moveto");
  return Val_unit;
}

value gr_current_point()	/* ML */
{
  invalid_argument("current_point");
  return Val_unit;
}

value gr_lineto(x, y)	/* ML */
     value x, y;
{
  invalid_argument("lineto");
  return Val_unit;
}

value gr_draw_arc(argv, argc)	/* ML */
     value * argv;
     int argc;
{
  invalid_argument("draw_arc");
  return Val_unit;
}

value gr_draw_char(c)	/* ML */
     value c;
{
  invalid_argument("draw_char");
  return Val_unit;
}

value gr_draw_string(s)	/* ML */
     value s;
{
  invalid_argument("draw_string");
  return Val_unit;
}

value gr_text_size(s)	/* ML */
     value s;
{
  invalid_argument("text_size");
  return Val_unit;
}

value gr_fill_rect(vx, vy, vw, vh)	/* ML */
     value vx, vy, vw, vh;
{
  invalid_argument("fill_rect");
  return Val_unit;
}

value gr_fill_arc(argv, argc)	/* ML */
     value * argv;
     int argc;
{
  invalid_argument("fill_arc");
  return Val_unit;
}

value gr_fill_poly(v)	/* ML */
     value v;
{
  invalid_argument("fill_poly");
  return Val_unit;
}

value gr_draw_image(image, vx, vy)	/* ML */
     struct image * image;
     value vx, vy;
{
  invalid_argument("draw_image");
  return Val_unit;
}

value gr_create_image(vw, vh)		/* ML */
	value vw, vh;
{
  invalid_argument("create_image");
  return Val_unit;
}

value gr_blit_image(image, vx, vy)	/* ML */
	struct image * image;
	value vx, vy;
{
  invalid_argument("blit_image");
  return Val_unit;
}

value gr_make_image(color_matrix)	/* ML */
     value color_matrix;
{
  invalid_argument("make_image");
  return Val_unit;
}

value gr_dump_image(image)	/* ML */
     struct image * image;
{
  invalid_argument("dump_image");
  return Val_unit;
}

value gr_wait_event(events)     /* ML */
     value events;
{
  invalid_argument("wait_event");
  return Val_unit;
}

value gr_sound(freq, duration)	/* ML */
	value freq, duration;
{
  invalid_argument("sound");
  return Val_unit;
}

/////////////////////////////////////////////////////////////////////////////
#endif	// HAS_UI
/////////////////////////////////////////////////////////////////////////////

