/* Switch immediate operands from little endian to big endian. */

#include "config.h"
#include "misc.h"
#include "mlvalues.h"
#include "instruct.h"
#include "reverse.h"

/* We don't need this code if 1- the machine is little-endian, or 2- the
   machine has alignment constraints, in which case the interpreter does word
   accesses byte per byte, in little-endian mode, regardless of the
   native endianness.
*/

#if defined(CAML_BIG_ENDIAN) && !defined(CAML_ALIGNMENT)

void fixup_endianness(p, len)
     register code_t p;
     asize_t len;
{
  register code_t q;
  int n;

  q = p + len;
  while(p < q) {
    switch(*p++) {
      /* Instructions with one one-byte + one two-byte argument */
    case BRANCHIFNEQTAG: case C_CALLN:
      p++; /* fall through */
      /* Instructions with a two-byte immediate argument */
    case GETGLOBAL: case SETGLOBAL: 
    case PUSH_GETGLOBAL_APPLY: case PUSH_GETGLOBAL_APPTERM:
    case CONSTSHORT:
    case CUR: case LETREC1: case PUSHTRAP:
    case BRANCH: case BRANCHIF: case BRANCHIFNOT: case POPBRANCHIFNOT:
    case BRANCHIFEQ: case BRANCHIFNEQ: case BRANCHIFLT:
    case BRANCHIFGT: case BRANCHIFLE: case BRANCHIFGE:
    case C_CALL1: case C_CALL2: case C_CALL3: case C_CALL4:
    case C_CALL5:
      Reverse_short(p);
      p += 2;
      break;
      /* Instructions with a four-byte immediate argument */
    case MAKEBLOCK:
      Reverse_word(p);
      p += 4;
      break;
      /* Instructions with two two-byte immediate arguments */
    case BRANCHINTERVAL:
      Reverse_short(p);
      Reverse_short(p+2);
      p += 4;
      break;
      /* Special case for switch */
    case SWITCH:
      n = *p++;
      while (n > 0) {
        Reverse_short(p);
        p += 2;
        n--;
      }
      break;
      /* Instructions with a one-byte immediate argument */
    case CONSTBYTE: case MAKEBLOCK1: case MAKEBLOCK2: case MAKEBLOCK3:
    case MAKEBLOCK4: case ACCESS: case ATOM: case GETFIELD: case SETFIELD:
    case DUMMY: case ENDLET: case FLOATOP:
      p++;
      break;
    }
  }
}

#endif
