/* Buffered input/output */

#ifndef _io_
#define _io_


#include "misc.h"
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

struct channel * open_descr P((int));
value flush P((struct channel *));
void putword P((struct channel *, uint32));
void putblock P((struct channel *, char *, unsigned));
unsigned char refill P((struct channel *));
uint32 getword P((struct channel *));
unsigned getblock P((struct channel *, char *, unsigned));
int really_getblock P((struct channel *, char *, unsigned long));
value close_in P((struct channel *));


#endif /* _io_ */
