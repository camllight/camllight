#include "libgraph.h"

value gr_sound(vfreq, vdur)
     value vfreq, vdur;
{
  XKeyboardControl kbdcontrol;

  kbdcontrol.bell_pitch = Int_val(vfreq);
  kbdcontrol.bell_duration = Int_val(vdur);
  XChangeKeyboardControl(grdisplay, KBBellPitch | KBBellDuration,
                         &kbdcontrol);
  XBell(grdisplay, 0);
  kbdcontrol.bell_pitch = -1;   /* restore default value */
  kbdcontrol.bell_duration = -1; /* restore default value */
  XChangeKeyboardControl(grdisplay, KBBellPitch | KBBellDuration,
                         &kbdcontrol);
  XFlush(grdisplay);
  return Val_unit;
}


