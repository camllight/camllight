#ifndef _alloc_
#define _alloc_


#include "mlvalues.h"

#ifdef ANSI
extern value alloc(mlsize_t, tag_t);
extern value alloc_tuple(mlsize_t);
extern value alloc_string(mlsize_t);
extern value alloc_final(mlsize_t, final_fun, mlsize_t, mlsize_t);
extern value copy_string(char *);
extern value copy_string_array(char **);
extern value copy_double(double);
extern value alloc_array(value (*funct)(value), char ** array);
extern int convert_flag_list(value, int *);
#else
value alloc();
value alloc_tuple();
value alloc_string();
value alloc_final();
value copy_string();
value copy_string_array();
value copy_double();
value alloc_array();
int convert_flag_list();
#endif


#endif /* _alloc_ */
