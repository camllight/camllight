/* lr0.c */
void allocate_itemsets(void);
void allocate_storage(void);
void append_states(void);
void free_storage(void);
void generate_states(void);
int get_state(int symbol);
void initialize_states(void);
void new_itemsets(void);
core *new_state(int symbol);
void show_cores(void);
void show_ritems(void);
void show_rrhs(void);
void show_shifts(void);
void save_shifts(void);
void save_reductions(void);
void set_derives(void);
void free_derives(void);
void set_nullable(void);
void free_nullable(void);
void lr0(void);