#ifdef DEBUG

#include <stdio.h>
#include "debugger.h"
#include "instruct.h"
#include "memory.h"
#include "mlvalues.h"
#include "opnames.h"
#include "stacks.h"
#include "unalignd.h"

code_t log_buffer[LOG_BUFFER_SIZE];
code_t * log_ptr;
int trace_flag;

/* Displaying a heap object */

long max_print = 100;
long max_print_depth = 10;

long print_cnt;

static void print_val(v, d)
     value v;
     long d;
{
  long n;
  value * p;

  if (d <= 0) {
    printf(".");
    return;
  }
  print_cnt--;
  if (print_cnt <= 0) {
    if (print_cnt == 0) printf("...");
    return;
  }
  if (Is_long(v))
    printf("%ld", Long_val(v));
  else if (!Is_in_heap (v) && !Is_young (v))
    printf("0x%lx", v);
  else switch(Tag_val(v)) {
    case String_tag:
      printf("\"%s\"", String_val(v));
      break;
    case Double_tag:
      printf("%g", Double_val(v));
      break;
    case Abstract_tag:
      printf("<abstract>");
      break;
    case Final_tag:
      printf("<finalized>");
      break;
    default:
      n = Tag_val(v);
      if (n < 26){
	printf ("%c", n + 'A');
      }else{
        printf("tag%ld", n);
      }
      n = Wosize_val(v);
      if (n > 0) {
        printf("(");
        p = &Field(v, 0);
        while (n > 1) {
          print_val(*p, d-1);
          printf(", ");
          p++;
          n--;
        }
        print_val(*p, d-1);
        printf(")");
      }
      break;
  }
}

void print_value(v)
	value v;
{
  print_cnt = max_print;
  print_val(v, max_print_depth);
  printf("\n");
}

extern code_t start_code;

void print_pc(pc)
     code_t pc;
{
  printf("%6d  ", pc - start_code);
}

/* Disassembling one instruction */

code_t disasm_instr(pc)
	code_t pc;
{
  int i;

  print_pc(pc);
  i = *pc++;
  if (i < 0 || i >= sizeof(names_of_instructions) / sizeof(char *)) {
    printf("???\n");
    return pc;
  }
  printf("%s ", names_of_instructions[i]);
  switch(i) {
/* instructions with a 1-byte immediate operand */
    case ACCESS: case DUMMY: case UPDATE: case ENDLET: case CONSTBYTE:
    case ATOM: case GETFIELD: case SETFIELD:
    case MAKEBLOCK1: case MAKEBLOCK2: case MAKEBLOCK3: case MAKEBLOCK4: 
    case C_CALL1: case C_CALL2: case C_CALL3: case C_CALL4: case C_CALL5:
      printf("%d", *pc++);
      break;
/* instructions with an unsigned 2-byte immediate operand */
    case GETGLOBAL: case SETGLOBAL:
    case PUSH_GETGLOBAL_APPLY: case PUSH_GETGLOBAL_APPTERM:
      printf("%d", u16(pc)); pc += 2; break;
/* instruction with a signed 2-byte immediate operand */
    case CONSTSHORT: 
      printf("%d", s16(pc)); pc += 2; break;
/* instructions with a 4-byte immediate operand */
    case MAKEBLOCK:
      printf("%ld", s32(pc)); pc += 4; break;
/* instructions with a displacement */
    case CUR: case LETREC1: case PUSHTRAP:
    case BRANCH: case BRANCHIF: case BRANCHIFNOT: case POPBRANCHIFNOT:
    case BRANCHIFEQ: case BRANCHIFNEQ: case BRANCHIFLT:
    case BRANCHIFGT: case BRANCHIFLE: case BRANCHIFGE:
    depl:
      { int orig = pc - start_code;
	printf("%d", orig + s16(pc));
	pc += 2;
        break;
      }
/* instruction with 2 displacements */
    case BRANCHINTERVAL:
      printf("%d, ", pc - start_code + s16(pc));
      pc += 2;
      printf("%d", pc - start_code + s16(pc));
      pc += 2;
      break;
/* instruction with tag and displacement */
    case BRANCHIFNEQTAG:
      printf("tag %d, ", (unsigned) *pc++);
      goto depl;
/* miscellaneous */
    case FLOATOP:
      printf("%s", names_of_float_instructions[*pc++]);
      break;
    case SWITCH:
      { int n;
        code_t orig;
	for (n = *pc++, orig = pc; n > 0; n--) {
	  printf("%d, ", orig + s16(pc) - start_code);
          pc += 2;
	}
      }
      break;
    }
  printf("\n");
  return pc;
}

void disasm(pc)
     code_t pc;
{
  int i;

  for (i = 0; i < 20; i++)
    pc = disasm_instr(pc);
}

void post_mortem(n)
	int n;
{
  code_t * p;

  if (n > LOG_BUFFER_SIZE) n = LOG_BUFFER_SIZE;
  for (p = log_buffer +
             (unsigned) (log_ptr - log_buffer - n) % LOG_BUFFER_SIZE;
       n > 0;
       n--) {
    disasm_instr(*p);
    p++;
    if (p >= log_buffer + LOG_BUFFER_SIZE) p = log_buffer;
  }
}

void failed_assert (expr, file, line)
     char *expr, *file;
     int line;
{
  fprintf (stderr, "Assertion failed: %s; file %s; line %d\n",
	   expr, file, line);
  exit (100);
}

static unsigned long seed = 0x12345;

unsigned long not_random ()
{
  seed = seed * 65537 + 12345;
  return seed;
}


#endif /* DEBUG */
