#include "libgraph.h"

XFontStruct * grfont = NULL;

static void gr_font(fontname)
     char * fontname;
{
  XFontStruct * font = XLoadQueryFont(grdisplay, fontname);
  if (font == NULL) gr_fail("cannot find font %s", fontname);
  if (grfont != NULL) XFreeFont(grdisplay, grfont);
  grfont = font;
  XSetFont(grdisplay, grwindow.gc, grfont->fid);
  XSetFont(grdisplay, grbstore.gc, grfont->fid);
}

value gr_set_font(fontname)
     value fontname;
{
  gr_check_open();
  gr_font(String_val(fontname));
  return Val_unit;
}

static void gr_draw_text(txt, len)
     char * txt;
     int len;
{
  if (grfont == NULL) gr_font(DEFAULT_FONT);
  XDrawString(grdisplay, grwindow.win, grwindow.gc,
              grx, Wcvt(gry) - grfont->descent + 1, txt, len);
  XDrawString(grdisplay, grbstore.win, grbstore.gc,
              grx, Bcvt(gry) - grfont->descent + 1, txt, len);
  grx += XTextWidth(grfont, txt, len);
  XFlush(grdisplay);
}

value gr_draw_char(chr)
     value chr;
{
  char str[1];
  gr_check_open();
  str[0] = Int_val(chr);
  gr_draw_text(str, 1);
  return Val_unit;
}
  
value gr_draw_string(str)
     value str;
{
  gr_check_open();
  gr_draw_text(String_val(str), string_length(str));
  return Val_unit;
}

value gr_text_size(str)
     value str;
{
  int width;
  value res;
  gr_check_open();
  if (grfont == NULL) gr_font(DEFAULT_FONT);
  width = XTextWidth(grfont, String_val(str), string_length(str));
  res = alloc_tuple(2);
  Field(res, 0) = Val_int(width);
  Field(res, 1) = Val_int(grfont->ascent + grfont->descent);
  return res;
}
