/* The bytecode interpreter */

#include <math.h>
#include "alloc.h"
#include "debugger.h"
#include "fail.h"
#include "instruct.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "prims.h"
#include "signals.h"
#include "stacks.h"
#include "str.h"
#include "unalignd.h"
#include "debugcom.h"
#ifdef HAS_UI
#include "ui.h"
#endif
#if defined (macintosh) && !defined (HAS_UI)
#include "spin.h"
#endif

#ifdef DEBUG
static long icount = 0;
static void stop_here () {}
#endif

/* Registers for the abstract machine */

/*	pc  	the code pointer
	asp  	the stack pointer for the argument stack (grows downward)
	rsp  	the stack pointer for the return stack (grows downward)
	tp  	pointer to the current trap frame
	env  	the remanent part (heap-allocated) of the environment
     cache_size the number of entries in the volatile part of the environment
	accu  	the accumulator

"asp" and "rsp" are local copies of the global variables
"extern_asp" and "extern_rsp".

*/

extern value global_data;
extern code_t start_code;

/* Other viewpoints on rsp */

#define retsp  ((struct return_frame *) rsp)
#define trapsp ((struct trap_frame   *) rsp)

#define push_ret_frame() \
  (rsp = (value *) ((char *) rsp - sizeof(struct return_frame)))
#define pop_ret_frame() \
  (rsp = (value *) ((char *) rsp + sizeof(struct return_frame)))
#define push_trap_frame() \
  (rsp = (value *) ((char *) rsp - sizeof(struct trap_frame)))
#define pop_trap_frame() \
  (rsp = (value *) ((char *) rsp + sizeof(struct trap_frame)))

/* Other viewpoints on pc (to read immediate operands) */

#define SHORT  (sizeof(short))
#define LONG   (sizeof(int32))
#define DOUBLE (sizeof(double))

#define s16pc s16(pc)
#define u16pc u16(pc)
#define s32pc s32(pc)
#define u32pc u32(pc)

/* The empty environment */

#define null_env Atom(0)

/* Code for returning from a signal handler */

unsigned char return_from_interrupt[] = { POP, RETURN };

/* To save and restore registers around GC calls */

#define Setup_for_gc							     \
  { push_ret_frame();							     \
    retsp->env = env;							     \
    retsp->cache_size = cache_size;					     \
    *--asp = accu;							     \
    extern_asp = asp; extern_rsp = rsp;					     \
  }

#define Restore_after_gc						     \
  { accu = *asp++;							     \
    env = retsp->env;							     \
    pop_ret_frame ();							     \
  }

/* To save and restore registers around C primitive calls. */

#define Setup_for_c_call                                                     \
  { push_ret_frame();                                                        \
    retsp->env = env;                                                        \
    retsp->cache_size = cache_size;                                          \
    extern_asp = asp;                                                        \
    extern_rsp = rsp;                                                        \
  }
#define Restore_after_c_call                                                 \
  { asp = extern_asp;                                                        \
    rsp = extern_rsp;                                                        \
    env = retsp->env;                                                        \
    pop_ret_frame();                                                         \
  }

/* To heap-allocate the whole environment */

#define heapify_env()							     \
{									     \
  mlsize_t env_size = Wosize_val(env);					     \
  mlsize_t new_size = env_size + cache_size;				     \
  value * from, * to;							     \
									     \
  Alloc_small(tmp,  new_size, 0);					     \
  for(to = Op_val(tmp); cache_size > 0; cache_size--) *to++ = *rsp++;	     \
  for(from = Op_val(env); env_size > 0; env_size--  ) *to++ = *from++;	     \
  env = tmp;								     \
}

/* GCC 2.0 has labels as first-class values. We take advantage of that
   to provide faster dispatch than the "switch" statement. */

#if defined(__GNUC__) && __GNUC__ >= 2 && !defined(DEBUG)
#define DIRECT_JUMP
#endif

/* The interpreter itself */

value interprete(prog)
     code_t prog;
{
/* Declarations for the registers of the abstract machine.
   The most heavily used registers come first.
   For reasonable performance, "pc" MUST reside in a register.
   Many ``optimizing'' compilers underestimate the importance of "pc",
   and don't put it in a register. 
   For GCC users, I've hand-assigned registers for some architectures. */

#if defined(__GNUC__) && defined(__mips__)
  register code_t pc asm("$16");
  register value accu asm("$17");
  register value * asp asm("$18");
  register value * rsp asm("$19");
#else
#if defined(__GNUC__) && defined(__alpha__)
  register code_t pc asm("$9");
  register value accu asm("$10");
  register value * asp asm("$11");
  register value * rsp asm("$12");
#else
#if defined(__GNUC__) && defined(sparc)
  register code_t pc asm("%l0");
  register value accu asm("%l1");
  register value * asp asm("%l2");
  register value * rsp asm("%l3");
#else
#if defined(__GNUC__) && defined(mc68000)
  register code_t pc asm("a5");
  register value accu;
  register value * asp;
  register value * rsp;
#else
#if defined(__GNUC__) && defined(i386)
#if defined(MSDOS)
  register code_t pc asm("si");
#else
  register code_t pc asm("%esi");
#endif
  register value accu;
  register value * asp;
  register value * rsp;
#else
  register code_t pc;
  register value accu;
  register value * asp;
  register value * rsp;
#endif
#endif
#endif
#endif
#endif
  int cur_instr;
  int cache_size;
  value env;
  value tmp;
  struct longjmp_buffer * initial_external_raise;
  int initial_rsp_offset;
  value * initial_c_roots_head;
  struct longjmp_buffer raise_buf;

#ifdef DIRECT_JUMP
  static void * jumptable[] = {
#   include "jumptbl.h"
  };
#endif

  asp = extern_asp;
  rsp = extern_rsp;
  pc = prog;
  env = null_env;
  cache_size = 0;
  accu = Val_long(0);

  initial_c_roots_head = c_roots_head;
  initial_external_raise = external_raise;
  initial_rsp_offset = (char *) ret_stack_high - (char *) rsp;

  if (setjmp(raise_buf.buf)) {
    c_roots_head = initial_c_roots_head;
    accu = exn_bucket;
    asp = extern_asp;
    rsp = extern_rsp;
    goto raise;
  }
  external_raise = &raise_buf;

#ifdef DEBUG
  log_ptr = log_buffer;
#endif

#ifdef DIRECT_JUMP
# define Instruct(name) lbl_##name
# define Next cur_instr = *pc++; goto *jumptable[cur_instr]
#else
# define Instruct(name) case name
# define Next break
#endif

#ifdef DIRECT_JUMP
  Next;                         /* Jump to the first instruction */
#else
  while (1) {
#ifdef DEBUG
    if (icount-- == 0) stop_here ();
    *log_ptr++ = pc;
    if (log_ptr >= log_buffer + LOG_BUFFER_SIZE) log_ptr = log_buffer;
    if (trace_flag) disasm_instr(pc);
    Assert(asp >= arg_stack_low);
    Assert(asp <= arg_stack_high);
    Assert(rsp >= ret_stack_low);
    Assert(rsp <= ret_stack_high);
#endif
    cur_instr = *pc++;
  decode_instruction:
    switch (cur_instr) {
#endif

    Instruct(STOP):
      extern_asp = asp;
      extern_rsp = rsp;
      external_raise = initial_external_raise;
      return accu;
      
    Instruct(CUR):
      if (cache_size) heapify_env();
      Alloc_small(accu, Closure_wosize, Closure_tag);
      Env_val(accu) = env;
      Code_val(accu) = pc + s16pc;
      pc += SHORT;
      Next;
      
    Instruct(APPLY):
    apply:
      push_ret_frame();
      retsp->pc = pc;
      retsp->env = env;
      retsp->cache_size = cache_size;
      *--rsp = *asp++;
      cache_size = 1;
      pc = Code_val(accu);
      env = Env_val(accu);
      goto check_stacks;
      
    Instruct(RETURN):
      if (*asp == MARK) {
	rsp += cache_size;
	asp++;
	pc = retsp->pc;
	env = retsp->env;
	cache_size = retsp->cache_size;
	pop_ret_frame();
        if (something_to_do) goto process_signal;
	Next;
      }
      /* fall through APPTERM */

    Instruct(APPTERM):
    appterm:
      rsp += cache_size;
      *--rsp = *asp++;
      cache_size = 1;
      pc = Code_val(accu);
      env = Env_val(accu);

    check_stacks:
      if (asp < arg_stack_threshold || rsp < ret_stack_threshold) {
        Setup_for_gc;
        realloc_stacks();
        rsp = extern_rsp;
        asp = extern_asp;
        Restore_after_gc;
      }
      /* fall through CHECK_SIGNALS */

    Instruct(CHECK_SIGNALS):
#ifdef PERIODIC_ACTION_FREQ
      { static int periodic_action_count = 1;
        if (--periodic_action_count == 0) {
          periodic_action_count = PERIODIC_ACTION_FREQ;
          ui_periodic_action();
        }
      }
#endif
      if (something_to_do) goto process_signal;
      Next;

    process_signal:
      something_to_do = 0;
      if (force_major_slice){
	force_major_slice = 0;
	Setup_for_gc;
	minor_collection ();
	Restore_after_gc;
      }
      /* If a signal arrives between the following two instructions,
         it will be lost. */
      tmp = (value) pending_signal;  pending_signal = 0;
      if ((int) tmp){
	push_ret_frame();
	retsp->pc = pc;
	retsp->env = env;
	retsp->cache_size = cache_size;
	*--asp = MARK;
	*--asp = accu;
	*--asp = MARK;
	env = Atom(0);
	push_ret_frame();
	retsp->pc = return_from_interrupt;
	retsp->env = env;
	retsp->cache_size = 0;
	*--rsp = Val_int((int) tmp);
	cache_size = 1;
	pc = pending_signal_handler;
      }
      Next;

    Instruct(PUSH_GETGLOBAL_APPLY):
      *--asp = accu;
      accu = Field(global_data, u16pc);
      pc += SHORT;
      goto apply;

    Instruct(PUSH_GETGLOBAL_APPTERM):
      *--asp = accu;
      accu = Field(global_data, u16pc);
      pc += SHORT;
      goto appterm;

    Instruct(GRAB):
      if (*asp != MARK) {
	*--rsp = *asp++;
	cache_size++;
      } else {
	if (cache_size) heapify_env();
	Alloc_small(accu, Closure_wosize, Closure_tag);
	Code_val(accu) = pc;
	Env_val(accu) = env;
	asp++;
	pc = retsp->pc;
	env = retsp->env;
	cache_size = retsp->cache_size;
	pop_ret_frame();
      }
      Next;
      
#define access(n) (cache_size > n ? rsp[n] : Field(env, n - cache_size))
#define access0() (cache_size > 0 ? rsp[0] : Field(env,0))

    Instruct(ACC0):
      accu = access0(); Next;
    Instruct(ACC1):
      accu = access(1); Next;
    Instruct(ACC2):
      accu = access(2); Next;
    Instruct(ACC3):
      accu = access(3); Next;
    Instruct(ACC4):
      accu = access(4); Next;
    Instruct(ACC5):
      accu = access(5); Next;
    Instruct(ACCESS):
      { int n = *pc++;
	accu = access(n);
	Next;
      }
      
    Instruct(LET):
      *--rsp = accu;
      cache_size++;
      Next;
      
    Instruct(DUMMY):
      { int n = *pc++;
	Assert (n > 0);
	Alloc_small(accu, n, 0);
	while (n--){
	  Field (accu, n) = Val_long (0);
	}
	Next;
      }

    Instruct(UPDATE):
      { mlsize_t n;
        tmp = *asp++;
        Tag_val (accu) = Tag_val (tmp);
        for (n = 0; n < Wosize_val (tmp); n++) {
          modify (&Field (accu, n), Field (tmp, n));
        }
        Next;
      }

    Instruct(LETREC1):		/* Replaces Dummy 1; Cur lbl; Update 0 */
      Alloc_small(accu, Closure_wosize, Closure_tag);
      Field(accu,0) = Field(accu,1) = Atom(0);
      *--rsp = accu;
      cache_size++;
      heapify_env();
      Code_val(accu) = pc + s16pc;
      Modify(&Env_val(accu), env);
      pc += SHORT;
      Next;
      
    Instruct(ENDLET1):
      if (cache_size != 0) {
	cache_size--; rsp++;
      } else {
	int i;
	value * from;
	i = Wosize_val(env);
        from = &Field(env, i);
        cache_size = i - 1;
        for (i = cache_size; i > 0; i--) *--rsp = *--from;
	env = null_env;
      }
      Next;
      
    Instruct(ENDLET):
      { int n = *pc++;
	if (cache_size >= n) {
	  cache_size -= n;
	  rsp += n;
	} else {
	  int i;
	  value * from;
	  n -= cache_size;
	  rsp += cache_size;
          i = Wosize_val(env);
	  cache_size = i - n;
          from = &Field(env, i);
	  for (i = cache_size; i > 0; i--) *--rsp = *--from;
	  env = null_env;
	}
	Next;
      }
      
    Instruct(PUSHTRAP):
      { value * src = rsp + cache_size;
	int i = cache_size;
	
	push_trap_frame();
	trapsp->pc = pc + s16pc;
	pc += SHORT;
	trapsp->env = env;
	trapsp->cache_size = cache_size + 2;
	trapsp->asp = asp;
	trapsp->tp = tp;
	tp = trapsp;
	while(i--) *--rsp = *--src;
	*--asp = MARK;
	Next;
      }

    raise:			/* An external raise jumps here */

    Instruct(RAISE):
      if ((value *) tp >= trap_barrier) debugger(TRAP_BARRIER);
      rsp = (value *) tp;
      if (rsp >= (value *)((char *) ret_stack_high - initial_rsp_offset)) {
        exn_bucket = accu;
        external_raise = initial_external_raise;
        longjmp(external_raise->buf, 1);
      }
      pc = trapsp->pc;
      env = trapsp->env;
      cache_size = trapsp->cache_size - 2;
      asp = trapsp->asp;
      tp = trapsp->tp;
      pop_trap_frame();
      *--rsp = accu;
      cache_size++;
      Next;
      
    Instruct(POPTRAP):
      if (something_to_do) {
        /* We must check here so that if a signal is pending and its
           handler triggers an exception, the exception is trapped
           by the current try...with, not the enclosing one. */
        pc--; /* restart the POPTRAP after processing the signal */
        goto process_signal;
      }
      rsp = (value *) tp;
      env = trapsp->env;
      cache_size = trapsp->cache_size - 2;
      asp = trapsp->asp;
      tp = trapsp->tp;
      pop_trap_frame();
      Next;
      
    Instruct(CONSTBYTE):
      accu = *pc++;  Next;
    Instruct(CONSTSHORT):
      accu = s16pc; pc += SHORT; Next;

    Instruct(ATOM0):
      accu = Atom(0); Next;
    Instruct(ATOM1):
      accu = Atom(1); Next;
    Instruct(ATOM2):
      accu = Atom(2); Next;
    Instruct(ATOM3):
      accu = Atom(3); Next;
    Instruct(ATOM4):
      accu = Atom(4); Next;
    Instruct(ATOM5):
      accu = Atom(5); Next;
    Instruct(ATOM6):
      accu = Atom(6); Next;
    Instruct(ATOM7):
      accu = Atom(7); Next;
    Instruct(ATOM8):
      accu = Atom(8); Next;
    Instruct(ATOM9):
      accu = Atom(9); Next;
    Instruct(ATOM):
      accu = Atom(*pc++); Next;
      
    Instruct(GETGLOBAL):
      accu = Field(global_data, u16pc);
      pc += SHORT;
      Next;
    Instruct(SETGLOBAL):
      modify(&Field(global_data, u16pc), accu);
      pc += SHORT;
      Next;
      
    Instruct(PUSH):
      *--asp = accu; Next;
    Instruct(POP):
      accu = *asp++; Next;
    Instruct(PUSHMARK):
      *--asp = MARK;
      Next;
      
#define branch() pc += s16pc
#define cond_branch(condition) if (condition) branch(); else pc += 2

    Instruct(BRANCH):
      branch(); Next;
    Instruct(BRANCHIF):
      if (Tag_val(accu) != 0) branch(); else pc += SHORT;
      Next;
    Instruct(BRANCHIFNOT):
      if (Tag_val(accu) == 0) branch(); else pc += SHORT;
      Next;
    Instruct(POPBRANCHIFNOT):
      tmp = accu;
      accu = *asp++;
      if (Tag_val(tmp) == 0) branch(); else pc += SHORT;
      Next;
    Instruct(BRANCHIFNEQTAG):
      if (Tag_val(accu) != *pc++) branch(); else pc += SHORT;
      Next;
    Instruct(SWITCH):
      Assert(Long_val(accu) >= 0 && Long_val(accu) < *pc);
      pc++;
      pc += s16(pc + accu - 1);
      Next;
    Instruct(BOOLNOT):
      accu = Atom(Tag_val(accu) == 0); Next;
      
    Instruct(GETFIELD0):
      accu = Field(accu,0); Next;
    Instruct(GETFIELD1):
      accu = Field(accu,1); Next;
    Instruct(GETFIELD2):
      accu = Field(accu,2); Next;
    Instruct(GETFIELD3):
      accu = Field(accu,3); Next;
    Instruct(GETFIELD):
      accu = Field(accu,*pc++); Next;
      
    Instruct(SETFIELD0):
      tmp = 0;
    setfield:
      { value * ptr;
        ptr = &Field(accu, tmp);
        tmp = *asp++;
        Modify(ptr, tmp);
        accu = Atom(0);
      }
      Next;
    Instruct(SETFIELD1):
      tmp = 1;
      goto setfield;
    Instruct(SETFIELD2):
      tmp = 2;
      goto setfield;
    Instruct(SETFIELD3):
      tmp = 3;
      goto setfield;
    Instruct(SETFIELD):
      tmp = *pc++;
      goto setfield;
      
    Instruct(MAKEBLOCK1):
      Alloc_small(tmp, 1, *pc);
      pc++;
      Field(tmp,0) = accu;
      accu = tmp;
      Next;
    Instruct(MAKEBLOCK2):
      Alloc_small(tmp, 2, *pc);
      pc++;
      Field(tmp,0) = accu;
      Field(tmp,1) = *asp++;
      accu = tmp;
      Next;
    Instruct(MAKEBLOCK3):
      Alloc_small(tmp, 3, *pc);
      pc++;
      Field(tmp,0) = accu;
      Field(tmp,1) = *asp++;
      Field(tmp,2) = *asp++;
      accu = tmp;
      Next;
    Instruct(MAKEBLOCK4):
      Alloc_small(tmp, 4, *pc);
      pc++;
      Field(tmp,0) = accu;
      Field(tmp,1) = *asp++;
      Field(tmp,2) = *asp++;
      Field(tmp,3) = *asp++;
      accu = tmp;
      Next;
    Instruct(MAKEBLOCK):
      { header_t hdr;
        mlsize_t size;
	tag_t tag;
	value * to;
	
	hdr = u32pc;
	pc += LONG;
	size = Wosize_hd(hdr);
	tag = Tag_hd(hdr);
        if (size < Max_young_wosize) {
          Alloc_small(tmp, size, tag);
          to = &Field(tmp, 0);
          *to++ = accu;
          for (size--; size > 0; size--) *to++ = *asp++;
          accu = tmp;
        } else {
          Setup_for_gc;
          tmp = alloc_shr (size, tag);
          Restore_after_gc;
          to = &Field(tmp, 0);
          initialize (to++, accu);
          for (size--; size > 0; size--) initialize (to++, *asp++);
          accu = tmp;
        }
	Next;
      }
      
    Instruct(TAGOF):
      accu = Val_long(Tag_val(accu));
      Next;

    Instruct(C_CALL1):
      Setup_for_c_call;
      accu = (cprim[u16pc])(accu);
      Restore_after_c_call;
      pc += SHORT;
      Next;
    Instruct(C_CALL2):
      Setup_for_c_call;
      accu = (cprim[u16pc])(accu, asp[0]);
      Restore_after_c_call;
      pc += SHORT;
      asp += 1;
      Next;
    Instruct(C_CALL3):
      Setup_for_c_call;
      accu = (cprim[u16pc])(accu, asp[0], asp[1]);
      Restore_after_c_call;
      pc += SHORT;
      asp += 2;
      Next;
    Instruct(C_CALL4):
      Setup_for_c_call;
      accu = (cprim[u16pc])(accu, asp[0], asp[1], asp[2]);
      Restore_after_c_call;
      pc += SHORT;
      asp += 3;
      Next;
    Instruct(C_CALL5):
      Setup_for_c_call;
      accu = (cprim[u16pc])(accu, asp[0], asp[1], asp[2], asp[3]);
      Restore_after_c_call;
      pc += SHORT;
      asp += 4;
      Next;
    Instruct(C_CALLN):
      { int n = *pc++;
        *--asp = accu;
        Setup_for_c_call;
        accu = (cprim[u16pc])(asp, n);
        Restore_after_c_call;
        pc += SHORT;
        asp += n;
        Next; }
      
    Instruct(NEGINT):
      accu = 2 - accu; Next;
    Instruct(SUCCINT):
      accu += 2; Next;
    Instruct(PREDINT):
      accu -= 2; Next;
    Instruct(ADDINT):
      accu += *asp++ - 1; Next;
    Instruct(SUBINT):
      accu -= *asp++ - 1; Next;
    Instruct(MULINT):
      accu = 1 + (accu >> 1) * (*asp++ - 1); Next;
    Instruct(DIVINT):
      tmp = *asp++ - 1;
      if (tmp == 0) {
        accu = Atom(ZERO_DIVIDE_EXN);
        goto raise;
      }
      accu = Val_long((accu - 1) / tmp);
      Next;
    Instruct(MODINT):
      tmp = *asp++ - 1;
      if (tmp == 0) {
        accu = Atom(ZERO_DIVIDE_EXN);
        goto raise;
      }
      accu = 1 + (accu - 1) % tmp;
      Next;
    Instruct(ANDINT):
      accu &= *asp++; Next;
    Instruct(ORINT):
      accu |= *asp++; Next;
    Instruct(XORINT):
      accu = 1 + (accu ^ *asp++); Next;
    Instruct(SHIFTLEFTINT):
      accu = 1 + ((accu - 1) << Long_val(*asp++)); Next;
    Instruct(SHIFTRIGHTINTSIGNED):
      accu = 1 | ((accu - 1) >> Long_val(*asp++)); Next;
    Instruct(SHIFTRIGHTINTUNSIGNED):
      accu = 1 | ((unsigned long)(accu - 1) >> Long_val(*asp++)); Next;
      
#define inttest(name1,name2,tst)					     \
    Instruct(name1):							     \
      accu = Atom(accu tst *asp++);					     \
      Next;								     \
    Instruct(name2):							     \
      if (accu tst *asp++) { branch(); } else { pc += SHORT; }               \
      Next;
      
      inttest(EQ,BRANCHIFEQ,==);
      inttest(NEQ,BRANCHIFNEQ,!=);
      inttest(LTINT,BRANCHIFLT,<);
      inttest(GTINT,BRANCHIFGT,>);
      inttest(LEINT,BRANCHIFLE,<=);
      inttest(GEINT,BRANCHIFGE,>=);

    Instruct(BRANCHINTERVAL):
      { value low_bound, high_bound;
        high_bound = accu;
        low_bound = *asp++;
        accu = *asp++;
        if (accu < low_bound) {
          branch();
          Next;
        }
        pc += SHORT;
        if (accu > high_bound) {
          branch();
          Next;
        } 
        pc += SHORT;
        accu = accu - low_bound + 1;
        Next;
      }

    Instruct(INCR):
      Field(accu, 0) += 2; accu = Atom(0); Next;
    Instruct(DECR):
      Field(accu, 0) -= 2; accu = Atom(0); Next;

    Instruct(FLOATOP):
      { Alloc_small(tmp, Double_wosize, Double_tag);
	switch(*pc++) {
	case FLOATOFINT: 
	  Store_double_val(tmp, (double) Long_val(accu)); break;
	case NEGFLOAT:
	  Store_double_val(tmp, -Double_val(accu)); break;
	case ADDFLOAT:
	  Store_double_val(tmp, Double_val(accu) + Double_val(*asp++)); break;
	case SUBFLOAT:
	  Store_double_val(tmp, Double_val(accu) - Double_val(*asp++)); break;
	case MULFLOAT: 
	  Store_double_val(tmp, Double_val(accu) * Double_val(*asp++)); break;
	case DIVFLOAT:
	  Store_double_val(tmp, Double_val(accu) / Double_val(*asp++)); break;
	}
	accu = tmp;
	Next;
      }
      
    Instruct(INTOFFLOAT):
      accu = Val_long((long)Double_val(accu)); Next;
      
#define floattest(name, tst)    					     \
    Instruct(name):							     \
      accu = Atom(Double_val(accu) tst Double_val(*asp++));		     \
      Next;
      
      floattest(EQFLOAT,==);
      floattest(NEQFLOAT,!=);
      floattest(LTFLOAT,<);
      floattest(GTFLOAT,>);
      floattest(LEFLOAT,<=);
      floattest(GEFLOAT,>=);
      
    Instruct(STRINGLENGTH):
      accu = Val_long(string_length(accu));
      Next;
    Instruct(GETSTRINGCHAR):
      accu = Val_long(Byte_u(accu, Long_val(*asp++)));
      Next;
    Instruct(SETSTRINGCHAR):
      Byte_u(accu, Long_val(asp[0])) = Long_val(asp[1]);
      accu = Atom(0);
      asp += 2;
      Next;

#define stringtest(name, tst)                                                \
    Instruct(name):                                                          \
      accu = Atom(compare_strings(accu, *asp++) tst Val_long(0));            \
      Next;
      
      stringtest(EQSTRING,==);
      stringtest(NEQSTRING,!=);
      stringtest(LTSTRING,<);
      stringtest(GTSTRING,>);
      stringtest(LESTRING,<=);
      stringtest(GESTRING, >=);

    Instruct(MAKEVECTOR):
      { mlsize_t size = Long_val(accu);
        if (size == 0)
          accu = Atom(0);
        else if (size < Max_young_wosize){
	  Alloc_small (accu, size, 0);
	  do {size--; Field (accu, size) = *asp;} while (size != 0);
	}else if (Is_block (*asp) && Is_young (*asp)){
	  Setup_for_gc;
	  minor_collection ();
	  tmp = alloc_shr (size, 0);
	  Restore_after_gc;
          accu = tmp;
	  do {size--; Field (accu, size) = *asp;} while (size != 0);
	}else{
	  Setup_for_gc;
	  tmp = alloc_shr (size, 0);
	  Restore_after_gc;
          accu = tmp;
	  do {size--; initialize(&Field(accu, size), *asp);} while (size != 0);
	}
        asp++;
	Next;
      }
    Instruct(VECTLENGTH):
      accu = Val_long(Wosize_val(accu));
      Next;
    Instruct(GETVECTITEM):
      accu = Field(accu, Long_val(*asp++));
      Next;
    Instruct(SETVECTITEM):
      tmp = Long_val(*asp++);
      goto setfield;

    Instruct(BREAK):
      Setup_for_gc;
      retsp->pc = pc - 1;
      cur_instr = debugger(BREAKPOINT);
      if (cur_instr == -1) cur_instr = pc[-1];
      Restore_after_gc;
#ifdef DIRECT_JUMP
      goto *jumptable[cur_instr & 0x7F];
#else
      cur_instr &= 0x7F;
      goto decode_instruction;
#endif

#ifdef DIRECT_JUMP
    lbl_EVENT:
#else
    default:
#endif
      if (--event_count == 0) {
        Setup_for_gc;
        retsp->pc = pc - 1;
        debugger(EVENT);
        Restore_after_gc;
      }
#ifdef DIRECT_JUMP
      goto *jumptable[cur_instr & 0x7F];
#else
      cur_instr &= 0x7F;
      goto decode_instruction;
#endif

#ifndef DIRECT_JUMP
    }
  }
#endif
}

static unsigned char callback_code [] = { POP, APPLY, STOP };

value callback(closure, argument)
     value closure, argument;
{
  *--extern_asp = MARK;
  *--extern_asp = argument;
  *--extern_asp = closure;
  return interprete(callback_code);
}
