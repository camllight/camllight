#include "libgraph.h"

value gr_point_color(vx, vy)
     value vx, vy;
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  XImage * im;
  int rgb;

  gr_check_open();
  im = XGetImage(grdisplay, grbstore.win, x, Bcvt(y), 1, 1, (-1), ZPixmap);
  rgb = gr_rgb_pixel(XGetPixel(im, 0, 0));
  XDestroyImage(im);
  return Val_int(rgb);
}

                     
