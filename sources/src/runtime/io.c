/* Buffered input/output. */

#include <fcntl.h>
#ifdef __TURBOC__
#include <io.h>
#endif
#include "alloc.h"
#include "fail.h"
#include "io.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "signals.h"
#include "sys.h"

/* Common functions. */

struct channel * open_descriptor(fd)       /* ML */
     value fd;
{
  struct channel * channel;

  channel = (struct channel *) stat_alloc(sizeof(struct channel));
  channel->fd = Long_val(fd);
  channel->offset = 0;
  channel->curr = channel->max = channel->buff;
  channel->end = channel->buff + IO_BUFFER_SIZE;
  return channel;
}

value channel_descriptor(channel)   /* ML */
     struct channel * channel;
{
  return Val_long(channel->fd);
}

value channel_size(channel)      /* ML */
     struct channel * channel;
{
  long end;

  end = lseek(channel->fd, 0, 2);
  if (end == -1) sys_error();
  if (lseek(channel->fd, channel->offset, 0) != channel->offset) sys_error();
  return Val_long(end);
}

/* Output */

static void really_write(fd, p, n)
     int fd;
     char * p;
     int n;
{
  int retcode;
  while (n > 0) {
    retcode = write(fd, p, n);
    if (retcode == -1) sys_error();
    p += retcode;
    n -= retcode;
  }
}   

value flush(channel)            /* ML */
     struct channel * channel;
{
  int n;
  n = channel->max - channel->buff;
  if (n > 0) {
    really_write(channel->fd, channel->buff, n);
    channel->offset += n;
    channel->curr = channel->buff;
    channel->max  = channel->buff;
  }
  return Atom(0);
}

value output_char(channel, ch)  /* ML */
     struct channel * channel;
     value ch;
{
  putch(channel, Long_val(ch));
  return Atom(0);
}

void putword(channel, w)
     struct channel * channel;
     long w;
{
  putch(channel, w >> 24);
  putch(channel, w >> 16);
  putch(channel, w >> 8);
  putch(channel, w);
}

value output_int(channel, w)    /* ML */
     struct channel * channel;
     value w;
{
  putword(channel, Long_val(w));
  return Atom(0);
}

void putblock(channel, p, n)
     struct channel * channel;
     char * p;
     unsigned n;
{
  unsigned m;

  m = channel->end - channel->curr;
  if (channel->curr == channel->buff && n >= m) {
    really_write(channel->fd, p, n);
    channel->offset += n;
  } else if (n <= m) {
    bcopy(p, channel->curr, n);
    channel->curr += n;
    if (channel->curr > channel->max) channel->max = channel->curr;
  } else {
    bcopy(p, channel->curr, m);
    p += m;
    n -= m;
    m = channel->end - channel->buff;
    really_write(channel->fd, channel->buff, m);
    channel->offset += m;
    if (n <= m) {
      bcopy(p, channel->buff, n);
      channel->curr = channel->max = channel->buff + n;
    } else {
      really_write(channel->fd, p, n);
      channel->offset += n;
      channel->curr = channel->max = channel->buff;
    }
  }
}

value output(channel, buff, start, length) /* ML */
     value channel, buff, start, length;
{
  putblock((struct channel *) channel,
           &Byte(buff, Long_val(start)),
           (unsigned) Long_val(length));
  return Atom(0);
}

value seek_out(channel, pos)    /* ML */
     struct channel * channel;
     value pos;
{
  long dest;

  dest = Long_val(pos);
  if (dest >= channel->offset &&
      dest <= channel->offset + channel->max - channel->buff) {
    channel->curr = channel->buff + dest - channel->offset;
  } else {
    flush(channel);
    if (lseek(channel->fd, dest, 0) != dest) sys_error();
    channel->offset = dest;
  }
  return Atom(0);
}

value pos_out(channel)          /* ML */
     struct channel * channel;
{
  return Val_long(channel->offset + channel->curr - channel->buff);
}

value close_out(channel)     /* ML */
     struct channel * channel;
{
  flush(channel);
  close(channel->fd);
  stat_free((char *) channel);
  return Atom(0);
}

/* Input */

static int really_read(fd, p, n)
     int fd;
     char * p;
     unsigned n;
{
  int retcode;

  enter_blocking_section();
#ifdef MSDOS
  retcode = msdos_read(fd, p, n);
#else
  retcode = read(fd, p, n);
#endif
  leave_blocking_section();
  if (retcode == -1) sys_error();
  return retcode;
}

unsigned char refill(channel)
     struct channel * channel;
{
  int n;

  n = really_read(channel->fd, channel->buff, IO_BUFFER_SIZE);
  if (n == 0) mlraise(Atom(END_OF_FILE_EXN));
  channel->offset += n;
  channel->max = channel->buff + n;
  channel->curr = channel->buff + 1;
  return (unsigned char)(channel->buff[0]);
}

value input_char(channel)       /* ML */
     struct channel * channel;
{
  unsigned char c;
  c = getch(channel);
  return Val_long(c);
}

long getword(channel)
     struct channel * channel;
{
  int i;
  long res;

  res = 0;
  for(i = 0; i < 4; i++) {
    res = (res << 8) + getch(channel);
  }
  return res;
}

value input_int(channel)        /* ML */
     struct channel * channel;
{
  return Val_long(getword(channel));
}

unsigned getblock(channel, p, n)
     struct channel * channel;
     char * p;
     unsigned n;
{
  unsigned m, l;

  m = channel->max - channel->curr;
  if (n <= m) {
    bcopy(channel->curr, p, n);
    channel->curr += n;
    return n;
  } else if (m > 0) {
    bcopy(channel->curr, p, m);
    channel->curr += m;
    return m;
  } else if (n < IO_BUFFER_SIZE) {
    l = really_read(channel->fd, channel->buff, IO_BUFFER_SIZE);
    channel->offset += l;
    channel->max = channel->buff + l;
    if (n > l) n = l;
    bcopy(channel->buff, p, n);
    channel->curr = channel->buff + n;
    return n;
  } else {
    l = really_read(channel->fd, p, n);
    channel->offset += l;
    return l;
  }
}


value input(channel, buff, start, length) /* ML */
     value channel, buff, start, length;
{
  return Val_long(getblock((struct channel *) channel,
                           &Byte(buff, Long_val(start)),
                           (unsigned) Long_val(length)));
}

value seek_in(channel, pos)     /* ML */
     struct channel * channel;
     value pos;
{
  long dest;

  dest = Long_val(pos);
  if (dest >= channel->offset - (channel->max - channel->buff) &&
      dest <= channel->offset) {
    channel->curr = channel->max - (channel->offset - dest);
  } else {
    if (lseek(channel->fd, dest, 0) != dest) sys_error();
    channel->offset = dest;
    channel->curr = channel->max = channel->buff;
  }
  return Atom(0);
}

value pos_in(channel)           /* ML */
     struct channel * channel;
{
  return Val_long(channel->offset - (channel->max - channel->curr));
}

value close_in(channel)     /* ML */
     struct channel * channel;
{
  close(channel->fd);
  stat_free((char *) channel);
  return Atom(0);
}

static value build_string(pref, start, end)
     value pref;
     char * start, * end;
{
  value res;
  mlsize_t preflen;

  if (Is_block(pref)) {
    Push_roots(r, 1);
    r[0] = pref;
    preflen = string_length(pref);
    res = alloc_string(preflen + end - start);
    bcopy(&Byte(r[0], 0), &Byte(res, 0), preflen);
    bcopy(start, &Byte(res, preflen), end - start);
    Pop_roots();
  } else {
    res = alloc_string(end - start);
    bcopy(start, &Byte(res, 0), end - start);
    return res;
  }
  return res;
}

value input_line(channel)       /* ML */
     struct channel * channel;
{
  char * start;
  int n;
  value res;

  Push_roots(r, 1);
#define prevstring r[0]
  prevstring = Val_long(0);
  start = channel->curr;
  do {
    if (channel->curr >= channel->max) { /* No more characters available */
      if (start > channel->buff) {       /* First, make as much room */
        bcopy(start, channel->buff, channel->max - start); /* as possible */
        n = start - channel->buff;       /* in the buffer */
        channel->curr -= n;
        channel->max  -= n;
        start = channel->buff;
      }
      if (channel->curr >= channel->end) { /* Buffer full? */
        prevstring = build_string(prevstring, start, channel->curr);
        start = channel->buff;             /* Flush it in the heap */
        channel->curr = channel->buff;
      }
      n = really_read(channel->fd, channel->curr, channel->end-channel->curr);
      if (n == 0) {
        Pop_roots();
        mlraise(Atom(END_OF_FILE_EXN));
      }
      channel->offset += n;
      channel->max = channel->curr + n;
    }
  } while (*(channel->curr)++ != '\n');
  res = build_string(prevstring, start, channel->curr - 1);
  Pop_roots();
  return res;
#undef prevstring
}
