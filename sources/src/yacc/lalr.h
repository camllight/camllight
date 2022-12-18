/* lalr.c */
void lalr(void);
void set_state_table(void);
void set_accessing_symbol(void);
void set_shift_table(void);
void set_reduction_table(void);
void set_maxrhs(void);
void initialize_LA(void);
void set_goto_map(void);
int map_goto(int state, int symbol);
void initialize_F(void);
void build_relations(void);
void add_lookback_edge(int stateno, int ruleno, int gotono);
short **transpose(short **R, int n);
void compute_FOLLOWS(void);
void compute_lookaheads(void);
void digraph(short **relation);
void traverse(register int i);
