#ifndef _signals_
#define _signals_


#ifdef ANSI

extern volatile int signal_is_pending;
extern volatile code_t signal_handler;
extern volatile int signal_number;
extern int in_blocking_section;

extern void execute_signal(void);
extern void enter_blocking_section(void);
extern void leave_blocking_section(void);

#else

extern int signal_is_pending;
extern code_t signal_handler;
extern int signal_number;
extern int in_blocking_section;

void execute_signal();
void enter_blocking_section();
void leave_blocking_section();

#endif


#endif /* _signals_ */

