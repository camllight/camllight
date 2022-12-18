/* mkpar.c */
void make_parser(void);
action *parse_actions(register int stateno);
action *get_shifts(int stateno);
action *add_reductions(int stateno, register action *actions);
action *add_reduce(register action *actions, register int ruleno, register int symbol);
void find_final_state(void);
void unused_rules(void);
void remove_conflicts(void);
void total_conflicts(void);
int sole_reduction(int stateno);
void defreds(void);
void free_action_row(register action *p);
void free_parser(void);
