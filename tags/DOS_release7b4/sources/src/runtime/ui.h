/* Entry points in the user interface */

#ifndef _ui_
#define _ui_


#include "misc.h"

void ui_periodic_action P((void));
int ui_read P((int fd, char * data, int len));
int ui_write P((int fd, char * data, int len));
void ui_gc_message P((char * fmt, unsigned long arg));
void ui_fatal_error P((char * fmt, char * arg));
int caml_main P((int argc, char ** argv));

#endif
