#ifndef _fail_
#define _fail_


#include <setjmp.h>
#include "misc.h"
#include "mlvalues.h"

#define OUT_OF_MEMORY_EXN 0     /* "exc","Out_of_memory",1 */
#define SYS_ERROR_EXN 1         /* "sys","Sys_error",1 */
#define FAILURE_EXN 2           /* "exc","Failure",3 */
#define INVALID_EXN 3           /* "exc","Invalid_argument",2 */
#define END_OF_FILE_EXN 4       /* "io","End_of_file",1 */
#define ZERO_DIVIDE_EXN 5       /* "int","Division_by_zero",1 */
#define BREAK_EXN 6             /* "sys","Break",2 */
#define NOT_FOUND_EXN 7         /* "exc","Not_found",4 */
#define UNIX_ERROR_EXN 8        /* "unix","Unix_error",1 */
#define GRAPHIC_FAILURE_EXN 9   /* "graphics","Graphic_failure",1 */
#define PARSE_FAILURE_EXN 10    /* "stream","Parse_failure",1 */
#define TCL_ERROR_EXN 11        /* "protocol","TkError",1 */

struct longjmp_buffer {
  jmp_buf buf;
};

extern struct longjmp_buffer * external_raise;
extern value exn_bucket;

void mlraise P((value));
void raise_with_arg P((tag_t tag, value arg));
void raise_with_string P((tag_t tag, char * msg));
void failwith P((char *));
void invalid_argument P((char *));
void raise_out_of_memory P((void));

#endif /* _fail_ */
