/* symtab.c */
int hash(char *name);
bucket *make_bucket(char *name);
bucket *lookup(char *name);
void create_symbol_table(void);
void free_symbol_table(void);
void free_symbols(void);
