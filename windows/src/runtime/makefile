# Makefile for the Caml Light runtime system

CC=gcc
CFLAGS=-I.. -O
LDFLAGS=-lm
PERL=C:\bin\perl.exe

OUTPUT=camlrun.exe

LIBS=

OBJS=interp.o misc.o stacks.o fix_code.o main.o fail.o signals.o debugcom.o \
  freelist.o major_gc.o minor_gc.o memory.o alloc.o roots.o \
  compare.o ints.o floats.o str.o io.o \
  extern.o externcp.o intern.o interncp.o \
  hash.o sys.o meta.o parsing.o lexing.o gc_ctrl.o \
  msdos.o graph.o aexpand.o nat.o
  
LIBNUM=bignum\libbignum.a

DOBJS=debugger.o

PRIMS=compare.c extern.c externcp.c floats.c gc_ctrl.c hash.c \
  intern.c interncp.c interp.c ints.c io.c lexing.c meta.c parsing.c \
  str.c sys.c graph.c nat.c

GENSOURCES=prims prims.c opnames.h jumptbl.h

all: camlrun.exe

camlrun.exe: libcaml.a prims.o
	$(CC) $(CFLAGS) -o camlrun.exe prims.o libcaml.a $(LIBS) $(LDFLAGS)

camlrund.exe: opnames.h libcaml.a prims.o $(DOBJS)
	$(CC) $(CFLAGS) -o camlrund.exe prims.o libcaml.a $(DOBJS) $(LIBS) $(LDFLAGS)

libcaml.a: $(OBJS) $(LIBNUM)
	copy $(LIBNUM) libcaml.a
	ar rc libcaml.a @<<
$(OBJS)
<<
	ranlib libcaml.a

install:
	cp camlrun.exe $(BINDIR)
	cp libcaml.a $(LIBDIR)
	sed -f ../tools/mkconfig.sed config.h > $(LIBDIR)\config.h
	cp mlvalues.h $(LIBDIR)
	cp alloc.h $(LIBDIR)
	cp misc.h $(LIBDIR)
	sed -f ../tools/mkmemory.sed memory.h > $(LIBDIR)\memory.h

prims: $(PRIMS)
    $(PERL) ../tools/findprim prims <<
$(PRIMS)
<<

prims.c: prims
	$(PERL) ../tools/mkprims prims > prims.c

opnames.h : instruct.h
	$(PERL) ../tools/mkopnam instruct.h > opnames.h

# jumptbl.h is required only if you have GCC 2.0 or later
jumptbl.h : instruct.h
	$(PERL) ../tools/mkjumptbl instruct.h > jumptbl.h

bignum\libbignum.a:
	cd bignum
	$(MAKE)	-f Makefile.gcc
	cd ..

!include "../tools/make-c.inc"

### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
alloc.o: alloc.c alloc.h misc.h config.h ../../config/m.h ../../config/s.h \
  mlvalues.h debugger.h major_gc.h freelist.h memory.h gc.h minor_gc.h stacks.h
compare.o: compare.c fail.h misc.h config.h ../../config/m.h ../../config/s.h \
  mlvalues.h memory.h gc.h major_gc.h freelist.h minor_gc.h str.h
debugcom.o: debugcom.c misc.h config.h ../../config/m.h ../../config/s.h \
  debugger.h mlvalues.h fail.h stacks.h memory.h gc.h major_gc.h freelist.h \
  minor_gc.h io.h debugcom.h
debugger.o: debugger.c
dummy.o: dummy.c fail.h misc.h config.h ../../config/m.h ../../config/s.h \
  mlvalues.h
extern.o: extern.c debugger.h misc.h config.h ../../config/m.h ../../config/s.h \
  mlvalues.h fail.h gc.h intext.h io.h memory.h major_gc.h freelist.h minor_gc.h
externcp.o: externcp.c debugger.h misc.h config.h ../../config/m.h \
  ../../config/s.h mlvalues.h fail.h gc.h intext.h io.h memory.h major_gc.h \
  freelist.h minor_gc.h reverse.h
fail.o: fail.c alloc.h misc.h config.h ../../config/m.h ../../config/s.h \
  mlvalues.h fail.h memory.h gc.h major_gc.h freelist.h minor_gc.h signals.h
fix_code.o: fix_code.c config.h ../../config/m.h ../../config/s.h misc.h \
  mlvalues.h instruct.h reverse.h
floats.o: floats.c alloc.h misc.h config.h ../../config/m.h ../../config/s.h \
  mlvalues.h debugger.h fail.h memory.h gc.h major_gc.h freelist.h minor_gc.h
freelist.o: freelist.c config.h ../../config/m.h ../../config/s.h debugger.h \
  misc.h mlvalues.h freelist.h gc.h gc_ctrl.h major_gc.h
gc_ctrl.o: gc_ctrl.c alloc.h misc.h config.h ../../config/m.h ../../config/s.h \
  mlvalues.h debugger.h gc.h gc_ctrl.h major_gc.h freelist.h minor_gc.h
graph.o: graph.c mlvalues.h config.h ../../config/m.h ../../config/s.h misc.h \
  fail.h
hash.o: hash.c mlvalues.h config.h ../../config/m.h ../../config/s.h misc.h \
  memory.h gc.h major_gc.h freelist.h minor_gc.h str.h
intern.o: intern.c debugger.h misc.h config.h ../../config/m.h ../../config/s.h \
  mlvalues.h fail.h gc.h intext.h io.h memory.h major_gc.h freelist.h minor_gc.h \
  reverse.h
interncp.o: interncp.c debugger.h misc.h config.h ../../config/m.h \
  ../../config/s.h mlvalues.h fail.h gc.h intext.h io.h memory.h major_gc.h \
  freelist.h minor_gc.h reverse.h
interp.o: interp.c alloc.h misc.h config.h ../../config/m.h ../../config/s.h \
  mlvalues.h debugger.h fail.h instruct.h memory.h gc.h major_gc.h freelist.h \
  minor_gc.h prims.h signals.h stacks.h str.h unalignd.h debugcom.h jumptbl.h
ints.o: ints.c alloc.h misc.h config.h ../../config/m.h ../../config/s.h \
  mlvalues.h fail.h memory.h gc.h major_gc.h freelist.h minor_gc.h
io.o: io.c alloc.h misc.h config.h ../../config/m.h ../../config/s.h mlvalues.h \
  debugger.h fail.h io.h memory.h gc.h major_gc.h freelist.h minor_gc.h signals.h \
  sys.h
lexing.o: lexing.c mlvalues.h config.h ../../config/m.h ../../config/s.h misc.h \
  stacks.h memory.h gc.h major_gc.h freelist.h minor_gc.h str.h
main.o: main.c alloc.h misc.h config.h ../../config/m.h ../../config/s.h \
  mlvalues.h debugcom.h debugger.h exec.h fail.h gc_ctrl.h globals.h intext.h io.h \
  stacks.h memory.h gc.h major_gc.h freelist.h minor_gc.h sys.h ../version.h
major_gc.o: major_gc.c config.h ../../config/m.h ../../config/s.h debugger.h \
  misc.h mlvalues.h fail.h freelist.h gc.h gc_ctrl.h globals.h major_gc.h roots.h
memory.o: memory.c debugger.h misc.h config.h ../../config/m.h ../../config/s.h \
  mlvalues.h fail.h freelist.h gc.h gc_ctrl.h major_gc.h memory.h minor_gc.h
meta.o: meta.c alloc.h misc.h config.h ../../config/m.h ../../config/s.h \
  mlvalues.h globals.h major_gc.h freelist.h memory.h gc.h minor_gc.h prims.h
minor_gc.o: minor_gc.c config.h ../../config/m.h ../../config/s.h debugger.h \
  misc.h mlvalues.h fail.h gc.h gc_ctrl.h major_gc.h freelist.h memory.h \
  minor_gc.h roots.h
misc.o: misc.c config.h ../../config/m.h ../../config/s.h debugger.h misc.h \
  mlvalues.h
msdos.o: msdos.c mlvalues.h config.h ../../config/m.h ../../config/s.h misc.h
parsing.o: parsing.c config.h ../../config/m.h ../../config/s.h mlvalues.h \
  misc.h memory.h gc.h major_gc.h freelist.h minor_gc.h alloc.h
prims.o: prims.c mlvalues.h config.h ../../config/m.h ../../config/s.h misc.h \
  prims.h
roots.o: roots.c debugger.h misc.h config.h ../../config/m.h ../../config/s.h \
  mlvalues.h memory.h gc.h major_gc.h freelist.h minor_gc.h stacks.h
signals.o: signals.c alloc.h misc.h config.h ../../config/m.h ../../config/s.h \
  mlvalues.h debugger.h signals.h stacks.h memory.h gc.h major_gc.h freelist.h \
  minor_gc.h
stacks.o: stacks.c config.h ../../config/m.h ../../config/s.h debugger.h misc.h \
  mlvalues.h debugcom.h fail.h stacks.h memory.h gc.h major_gc.h freelist.h \
  minor_gc.h
str.o: str.c alloc.h misc.h config.h ../../config/m.h ../../config/s.h \
  mlvalues.h debugger.h
sys.o: sys.c alloc.h misc.h config.h ../../config/m.h ../../config/s.h \
  mlvalues.h debugcom.h fail.h globals.h instruct.h signals.h stacks.h memory.h \
  gc.h major_gc.h freelist.h minor_gc.h
