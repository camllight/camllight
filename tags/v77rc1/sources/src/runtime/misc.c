#include <stdio.h>
#include "config.h"
#include "debugger.h"
#include "misc.h"
#ifdef HAS_UI
#include "ui.h"
#endif

int verb_gc;
int Volatile something_to_do = 0;
int Volatile force_major_slice = 0;

void urge_major_slice ()
{
  force_major_slice = 1;
  something_to_do = 1;
}

void gc_message (msg, arg)
     char *msg;
     unsigned long arg;
{
  if (verb_gc){
#ifdef HAS_UI
    ui_gc_message(msg, arg);
#else
    fprintf (stderr, msg, arg);
    fflush (stderr);
#endif
  }
}

void fatal_error (msg)
     char * msg;
{
  fatal_error_arg("%s", msg);
}

void fatal_error_arg (fmt, arg)
     char * fmt, * arg;
{
#ifdef NEED_FREE_ALL
  xfree_all();
#endif
#ifdef HAS_UI
  ui_fatal_error(fmt, arg);
#else
  fprintf (stderr, fmt, arg);
  exit(2);
#endif
}

#ifdef USING_MEMMOV

/* This should work on 64-bit machines as well as 32-bit machines.
   It assumes a long is the natural size for memory reads and writes.
*/
void memmov (dst, src, length)
     char *dst, *src;
     unsigned long length;
{
  unsigned long i;

  if ((unsigned long) dst <= (unsigned long) src){

      /* Copy in ascending order. */
    if (((unsigned long) src - (unsigned long) dst) % sizeof (long) != 0){

        /* The pointers are not equal modulo sizeof (long).
           Copy byte by byte. */
      for (; length != 0; length--){
	*dst++ = *src++;
      }
    }else{

        /* Copy the first few bytes. */
      i = (unsigned long) dst % sizeof (long);
      if (i != 0){
	i = sizeof (long) - i;              /* Number of bytes to copy. */
	if (i > length) i = length;         /* Never copy more than length.*/
	for (; i != 0; i--){
	  *dst++ = *src++; --length;
	}
      }                    Assert ((unsigned long) dst % sizeof (long) == 0);
                           Assert ((unsigned long) src % sizeof (long) == 0);

      /* Then copy as many entire words as possible. */
      for (i = length / sizeof (long); i > 0; i--){
	*(long *) dst = *(long *) src;
	dst += sizeof (long); src += sizeof (long);
      }

      /* Then copy the last few bytes. */
      for (i = length % sizeof (long); i > 0; i--){
	*dst++ = *src++;
      }
    }
  }else{                                       /* Copy in descending order. */
    src += length; dst += length;
    if (((unsigned long) dst - (unsigned long) src) % sizeof (long) != 0){

        /* The pointers are not equal modulo sizeof (long).
	   Copy byte by byte. */
      for (; length > 0; length--){
	*--dst = *--src;
      }
    }else{

        /* Copy the first few bytes. */
      i = (unsigned long) dst % sizeof (long);
      if (i > length) i = length;           /* Never copy more than length. */
      for (; i > 0; i--){
	*--dst = *--src; --length;
      }

        /* Then copy as many entire words as possible. */
      for (i = length / sizeof (long); i > 0; i--){
	dst -= sizeof (long); src -= sizeof (long);
	*(long *) dst = *(long *) src;
      }

        /* Then copy the last few bytes. */
      for (i = length % sizeof (long); i > 0; i--){
	*--dst = *--src;
      }
    }
  }
}

#endif /* USING_MEMMOV */

char * aligned_malloc (size, modulo)
     asize_t size;
     int modulo;
{
  char *raw_mem;
  unsigned long aligned_mem;
                                                 Assert (modulo < Page_size);
  raw_mem = (char *) xmalloc (size + Page_size);
  if (raw_mem == NULL) return NULL;
  raw_mem += modulo;		/* Address to be aligned */
  aligned_mem = (((unsigned long) raw_mem / Page_size + 1) * Page_size);
  return (char *) (aligned_mem - modulo);
}

#ifdef NEED_FREE_ALL

struct malloc_header {
  struct malloc_header * prev, * next;
};

static struct malloc_header malloc_chain = { &malloc_chain, &malloc_chain };

#define Header(b) ((struct malloc_header *)(b) - 1)

char * xmalloc(size)
     asize_t size;
{
  char * res = (char *) malloc(sizeof(struct malloc_header) + size);
  if (res == NULL) return NULL;
  res += sizeof(struct malloc_header);
  Header(res)->next = malloc_chain.next;
  malloc_chain.next->prev = Header(res);
  malloc_chain.next = Header(res);
  Header(res)->prev = &malloc_chain;
  return res;
}

void xfree(block)
     char * block;
{
  Header(block)->next->prev = Header(block)->prev;
  Header(block)->prev->next = Header(block)->next;
  free((char *) Header(block));
}

char * xrealloc(block, size)
     char * block;
     asize_t size;
{
  char * res;
  Header(block)->next->prev = Header(block)->prev;
  Header(block)->prev->next = Header(block)->next;
  res = (char *) realloc((char *)Header(block), 
                         sizeof(struct malloc_header) + size);
  if (res == NULL) return NULL;
  res += sizeof(struct malloc_header);
  Header(res)->next = malloc_chain.next;
  malloc_chain.next->prev = Header(res);
  malloc_chain.next = Header(res);
  Header(res)->prev = &malloc_chain;
  return res;
}

void xfree_all()
{
  struct malloc_header * p, * q;
  for (p = malloc_chain.next; p != &malloc_chain; /*nothing*/) {
    q = p->next;
    free((char *) p);
    p = q;
  }
}

#endif

