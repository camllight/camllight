#ifndef _gc_ctrl_
#define _gc_ctrl_


extern long
     stat_minor_words,
     stat_promoted_words,
     stat_major_words,
     stat_minor_collections,
     stat_major_collections,
     stat_heap_size;

#ifdef ANSI
extern void init_gc (long, long, int, int);
#else
void init_gc ();
#endif


#endif /* _gc_ctrl_ */
