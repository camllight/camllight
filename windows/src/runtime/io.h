/* Buffered input/output */

#ifndef _io_
#define _io_


#include "mlvalues.h"

#ifndef IO_BUFFER_SIZE
#define IO_BUFFER_SIZE 4096
#endif

struct channel {
  int fd;                       /* Unix file descriptor */
  long offset;                  /* Absolute position of fd in the file */
  char * curr;                  /* Current position in the buffer */
  char * max;                   /* Logical end of the buffer */
  char * end;                   /* Physical end of the buffer */
  char buff[IO_BUFFER_SIZE];    /* The buffer itself */
};

/* For an output channel:
     [offset] is the absolute position of the beginning of the buffer [buff].
   For an input channel:
     [offset] is the absolute position of the logical end of the buffer [max].
*/

#define putch(channel, ch)                                                    \
  { if ((channel)->curr >= (channel)->end) flush(channel);                    \
    *((channel)->curr)++ = (ch);                                              \
    if ((channel)->curr > (channel)->max) (channel)->max = (channel)->curr; }

#define getch(channel)                                                        \
  ((channel)->curr >= (channel)->max                                          \
   ? refill(channel)                                                          \
   : (unsigned char) *((channel))->curr++)

#ifdef ANSI

extern struct channel * open_descriptor(value);
extern value flush(struct channel *);
extern void putword(struct channel *, long);
extern void putblock(struct channel *, char *, unsigned);
extern unsigned char refill(struct channel *);
extern long getword(struct channel *);
extern unsigned getblock(struct channel *, char *, unsigned);
extern value close_in (struct channel *);

#else

struct channel * open_descriptor();
value flush();
void putword();
void putblock();
unsigned char refill();
long getword();
unsigned getblock();
value close_in ();

#endif


#endif /* _io_ */
