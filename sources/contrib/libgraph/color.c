#include "libgraph.h"

unsigned long gr_pixel_rgb(rgb)
     int rgb;

{
  unsigned int r, g, b;
  XColor color;
  if (rgb == 0) return grblack;
  if (rgb == 0xFFFFFF) return grwhite;
  r = (rgb >> 16) & 0xFF;
  g = (rgb >> 8) & 0xFF;
  b = rgb & 0xFF;
  color.red = r * 0x101;
  color.green = g * 0x101;
  color.blue = b * 0x101;
  XAllocColor(grdisplay, grcolormap, &color);
  return color.pixel;
}

int gr_rgb_pixel(pixel)
{
  XColor color;
  if (pixel == grblack) return 0;
  if (pixel == grwhite) return 0xFFFFFF;
  color.pixel = pixel;
  XQueryColor(grdisplay, grcolormap, &color);
  return
    ((color.red >> 8) << 16) + ((color.green >> 8) << 8) + (color.blue >> 8);
}

value gr_set_color(vrgb)
     value vrgb;
{
  gr_check_open();
  grcolor = gr_pixel_rgb(Int_val(vrgb));
  XSetForeground(grdisplay, grwindow.gc, grcolor);
  XSetForeground(grdisplay, grbstore.gc, grcolor);
  return Val_unit;
}


                                     


