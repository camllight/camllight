/* verbose.c */
void verbose(void);
void log_unused(void);
void log_conflicts(void);
void print_state(int state);
void print_conflicts(int state);
void print_core(int state);
void print_nulls(int state);
void print_actions(int stateno);
void print_shifts(register action *p);
void print_reductions(register action *p, register int defred);
void print_gotos(int stateno);
