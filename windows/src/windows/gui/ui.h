// ui.h

void ui_periodic_action (void);
#define PERIODIC_ACTION_FREQ 100
int ui_write (int fd, char * text, int len);
int ui_read (int fd, char * text, int len);
void ui_gc_message (char * fmt, unsigned long arg);
void ui_exit (int retcode);
void ui_fatal_error (char * fmt, char * arg);

// int caml_main (int argc, char ** argv);

