# Makefile for the Caml Light runtime system

OUTPUT=camlrun.exe

LIBS=-lgrx -lpc -lm

OBJS=interp.o misc.o stacks.o fix_code.o main.o fail.o signals.o debugcom.o \
  freelist.o major_gc.o minor_gc.o memory.o alloc.o roots.o \
  compare.o ints.o floats.o str.o io.o \
  extern.o externcp.o intern.o interncp.o \
  hash.o sys.o meta.o parsing.o lexing.o gc_ctrl.o \
  graph.o msdos.o

DOBJS=debugger.o

PRIMS=compare.c extern.c externcp.c floats.c gc_ctrl.c hash.c \
  intern.c interncp.c interp.c ints.c io.c lexing.c meta.c parsing.c \
  str.c sys.c \
  graph.c

GENSOURCES=primitives prims.c opnames.h jumptbl.h

all: camlrun.exe

camlrun.out: $(OBJS) prims.o
	$(CC) $(CFLAGS) -o camlrun.out @&&!
prims.o $(OBJS) $(LIBS)
!

camlrund.out: opnames.h $(OBJS) prims.o $(DOBJS) $(RUNTIME32)
	$(CC) $(CFLAGS) -o camlrund.out @&&!
prims.o $(OBJS) $(DOBJS) $(LIBS)
!

install:
	copy camlrun.exe $(BINDIR)

prims.c: $(PRIMS)
	perl ../tools/findprim prims <<!
$(PRIMS)
!
	perl ../tools/mkprims prims > prims.c

opnames.h : instruct.h
	perl ../tools/mkopnam instruct.h > opnames.h

# jumptbl.h is required only if you have GCC 2.0 or later
jumptbl.h : instruct.h
	perl ../tools/mkjumptbl instruct.h > jumptbl.h

!include "../tools/make-c.inc"

### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
alloc.o : alloc.c alloc.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  debugger.h major_gc.h freelist.h misc.h memory.h gc.h minor_gc.h stacks.h 
debugger.o : debugger.c 
equal.o : equal.c fail.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  memory.h gc.h major_gc.h freelist.h misc.h minor_gc.h str.h 
roots.o : roots.c debugger.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  memory.h gc.h major_gc.h freelist.h misc.h minor_gc.h stacks.h 
fail.o : fail.c alloc.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  fail.h memory.h gc.h major_gc.h freelist.h misc.h minor_gc.h signals.h 
fix_code.o : fix_code.c config.h ../../config/m.h ../../config/s.h misc.h mlvalues.h \
  instruct.h reverse.h 
floats.o : floats.c alloc.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  fail.h memory.h gc.h major_gc.h freelist.h misc.h minor_gc.h debugger.h 
freelist.o : freelist.c config.h ../../config/m.h ../../config/s.h debugger.h \
  mlvalues.h freelist.h gc.h major_gc.h misc.h 
hash.o : hash.c mlvalues.h config.h ../../config/m.h ../../config/s.h memory.h \
  gc.h major_gc.h freelist.h misc.h minor_gc.h str.h 
interp.o : interp.c alloc.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  debugger.h fail.h instruct.h memory.h gc.h major_gc.h freelist.h misc.h minor_gc.h \
  prims.h signals.h stacks.h str.h unalignd.h jumptbl.h 
intext.o : intext.c debugger.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  fail.h gc.h intext.h io.h memory.h major_gc.h freelist.h misc.h minor_gc.h reverse.h 
ints.o : ints.c alloc.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  fail.h memory.h gc.h major_gc.h freelist.h misc.h minor_gc.h 
io.o : io.c alloc.h mlvalues.h config.h ../../config/m.h ../../config/s.h fail.h \
  io.h memory.h gc.h major_gc.h freelist.h misc.h minor_gc.h signals.h sys.h 
main.o : main.c alloc.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  exec.h fail.h gc.h globals.h intext.h io.h misc.h stacks.h memory.h major_gc.h \
  freelist.h minor_gc.h sys.h 
major_gc.o : major_gc.c config.h ../../config/m.h ../../config/s.h debugger.h \
  mlvalues.h fail.h freelist.h gc.h globals.h major_gc.h misc.h roots.h 
lexing.o : lexing.c mlvalues.h config.h ../../config/m.h ../../config/s.h stacks.h \
  memory.h gc.h major_gc.h freelist.h misc.h minor_gc.h str.h 
memory.o : memory.c debugger.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  fail.h freelist.h gc.h major_gc.h misc.h memory.h minor_gc.h 
meta.o : meta.c alloc.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  globals.h major_gc.h freelist.h misc.h memory.h gc.h minor_gc.h prims.h 
minor_gc.o : minor_gc.c config.h ../../config/m.h ../../config/s.h debugger.h \
  mlvalues.h fail.h gc.h major_gc.h freelist.h misc.h memory.h minor_gc.h roots.h 
stacks.o : stacks.c config.h ../../config/m.h ../../config/s.h debugger.h mlvalues.h \
  fail.h misc.h stacks.h memory.h gc.h major_gc.h freelist.h minor_gc.h 
str.o : str.c alloc.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  debugger.h 
sys.o : sys.c config.h ../../config/m.h ../../config/s.h alloc.h mlvalues.h \
  fail.h globals.h instruct.h signals.h stacks.h memory.h gc.h major_gc.h freelist.h \
  misc.h minor_gc.h 
version.o : version.c 
misc.o : misc.c config.h ../../config/m.h ../../config/s.h debugger.h mlvalues.h \
  misc.h 
parsing.o : parsing.c config.h ../../config/m.h ../../config/s.h mlvalues.h \
  memory.h gc.h major_gc.h freelist.h misc.h minor_gc.h 
signals.o : signals.c mlvalues.h config.h ../../config/m.h ../../config/s.h \
  signals.h alloc.h stacks.h memory.h gc.h major_gc.h freelist.h misc.h minor_gc.h 
graph.o : graph.c mlvalues.h config.h ../../config/m.h ../../config/s.h alloc.h \
  memory.h gc.h major_gc.h freelist.h misc.h minor_gc.h fail.h 
msdos.o : msdos.c mlvalues.h config.h ../../config/m.h ../../config/s.h signals.h \
  instruct.h fail.h 
prims.o : prims.c mlvalues.h config.h ../../config/m.h ../../config/s.h prims.h 
