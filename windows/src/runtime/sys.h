#ifndef _sys_
#define _sys_


#ifdef ANSI
extern void sys_error (void);
extern void raise_pending_signal (void);
extern void sys_init (char **);
extern void sys_exit (value);
#else
void sys_error ();
void raise_pending_signal ();
void sys_init ();
void sys_exit ();
#endif


#endif /* _sys_ */
