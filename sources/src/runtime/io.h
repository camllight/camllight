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

struct channel *open_descr(int fd);
value open_descriptor(value fd);
value channel_descriptor(struct channel *channel);
value channel_size(struct channel *channel);
value flush(struct channel *channel);
value output_char(struct channel *channel, value ch);
void putword(struct channel *channel, uint32 w);
value output_int(struct channel *channel, value w);
void putblock(struct channel *channel, char *p, unsigned n);
value output(value channel, value buff, value start, value length);
value seek_out(struct channel *channel, value pos);
value pos_out(struct channel *channel);
value close_out(struct channel *channel);
unsigned char refill(struct channel *channel);
value input_char(struct channel *channel);
uint32 getword(struct channel *channel);
value input_int(struct channel *channel);
unsigned getblock(struct channel *channel, char *p, unsigned n);
int really_getblock(struct channel *chan, char *p, unsigned long n);
value input(value channel, value buff, value start, value length);
value seek_in(struct channel *channel, value pos);
value pos_in(struct channel *channel);
value close_in(struct channel *channel);
value input_scan_line(struct channel *channel);

#endif /* _io_ */
