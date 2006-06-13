# Makefile for the Caml Light runtime system

# These options are overriden when called from ../Makefile.
CC=cc
OPTS=#-fno-defer-pop
CFLAGS=-O2 $(OPTS) -I..
LIBS=

# Test to see whether ranlib exists on the machine
RANLIBTEST=test -f /usr/bin/ranlib -o -f /bin/ranlib

# How to invoke ranlib
RANLIB=ranlib

#========== End of configuration section


OBJS=interp.o misc.o stacks.o fix_code.o main.o fail.o signals.o debugcom.o \
  freelist.o major_gc.o minor_gc.o memory.o alloc.o roots.o \
  compare.o ints.o floats.o str.o io.o \
  extern.o externcp.o intern.o interncp.o \
  hash.o sys.o meta.o parsing.o lexing.o gc_ctrl.o

DOBJS=$(OBJS:.o=.d.o) debugger.d.o

PRIMS=compare.c extern.c externcp.c floats.c gc_ctrl.c hash.c \
  intern.c interncp.c interp.c ints.c io.c lexing.c meta.c parsing.c \
  str.c sys.c

all: camlrun libcaml.a

camlrun: $(OBJS) prims.o
	$(CC) $(CFLAGS) -o camlrun prims.o $(OBJS) $(LIBS) -lm

camlrund: $(DOBJS) prims.o
	$(CC) -g $(OPTS) -o camlrund prims.o $(DOBJS) $(LIBS) -lm

libcaml.a: $(OBJS)
	rm -f libcaml.a
	ar rc libcaml.a $(OBJS)
	if $(RANLIBTEST); then $(RANLIB) libcaml.a; else true; fi

libcamld.a: $(DOBJS)
	rm -f libcamld.a
	ar rc libcamld.a $(DOBJS)
	if $(RANLIBTEST); then $(RANLIB) libcamld.a; else true; fi

clean:
	rm -f camlrun camlrund *.o *.a
	rm -f primitives prims.c opnames.h jumptbl.h

install:
	cp camlrun $(BINDIR)
	strip $(BINDIR)/camlrun
	cp libcaml.a $(LIBDIR)
	if $(RANLIBTEST); then cd $(LIBDIR); $(RANLIB) libcaml.a; else true; fi
	cp mlvalues.h alloc.h misc.h $(LIBDIR)
	sed -e '/#include ".*\/m.h/r ../../config/m.h' \
            -e '/#include ".*\/s.h/r ../../config/s.h' \
            -e '/#include "/d' config.h > $(LIBDIR)/config.h
	sed -e '/#include ".*gc\.h"/d' \
	    -e '/#define Alloc_small/,/^}/d' \
	    -e '/Modify/,/^}/d' memory.h > $(LIBDIR)/memory.h

primitives : $(PRIMS)
	sed -n -e '/\/\* ML \*\//s/.* \([a-z0-9_][a-z0-9_]*\) *(.*/\1/p' \
                $(PRIMS) > primitives2
	sh -c 'if cmp -s primitives primitives2; \
        then rm primitives2; \
        else mv primitives2 primitives; \
        fi'

prims.c : primitives
	(echo '#include "mlvalues.h"'; \
	 echo '#include "prims.h"'; \
	 sed -e 's/.*/extern value &();/' primitives; \
	 echo 'c_primitive cprim[] = {'; \
	 sed -e 's/.*/  &,/' primitives; \
	 echo '  0 };'; \
         echo 'char * names_of_cprim[] = {'; \
	 sed -e 's/.*/  "&",/' primitives; \
	 echo '  0 };') > prims.c

opnames.h : instruct.h
	sed -e '/\/\*/d' \
            -e 's/enum /char * names_of_/' \
            -e 's/{$$/[] = {/' \
            -e 's/\([A-Z][A-Z_0-9]*\)/"\1"/g' instruct.h > opnames.h

# jumptbl.h is required only if you have GCC 2.0 or later
jumptbl.h : instruct.h
	awk -f ../tools/make-jumptbl instruct.h > jumptbl.h

.SUFFIXES: .d.o

.c.d.o:
	cd .debugobj; $(CC) -c -g -I.. -I../.. -DDEBUG $(OPTS) ../$<
	mv .debugobj/$*.o $*.d.o

depend : prims.c opnames.h jumptbl.h
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak;	 \
         gcc -MM -I.. $(OPTS) *.c; \
         gcc -MM -I.. $(OPTS) -DDEBUG *.c | sed -e 's/\.o/.d.o/') > Makefile
	rm Makefile.bak

### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
alloc.o: alloc.c alloc.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h debugger.h major_gc.h freelist.h memory.h \
 gc.h minor_gc.h stacks.h
compare.o: compare.c fail.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h memory.h gc.h major_gc.h freelist.h \
 minor_gc.h str.h
debugcom.o: debugcom.c misc.h config.h ../../config/m.h \
 ../../config/s.h debugger.h mlvalues.h fail.h stacks.h memory.h gc.h \
 major_gc.h freelist.h minor_gc.h io.h debugcom.h
debugger.o: debugger.c
extern.o: extern.c debugger.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h fail.h gc.h intext.h io.h memory.h \
 major_gc.h freelist.h minor_gc.h
externcp.o: externcp.c debugger.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h fail.h gc.h intext.h io.h memory.h \
 major_gc.h freelist.h minor_gc.h reverse.h
fail.o: fail.c alloc.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h fail.h memory.h gc.h major_gc.h \
 freelist.h minor_gc.h signals.h
fix_code.o: fix_code.c config.h ../../config/m.h ../../config/s.h \
 misc.h mlvalues.h instruct.h reverse.h
floats.o: floats.c alloc.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h debugger.h fail.h memory.h gc.h \
 major_gc.h freelist.h minor_gc.h
freelist.o: freelist.c config.h ../../config/m.h ../../config/s.h \
 debugger.h misc.h mlvalues.h freelist.h gc.h gc_ctrl.h major_gc.h
gc_ctrl.o: gc_ctrl.c alloc.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h debugger.h gc.h gc_ctrl.h major_gc.h \
 freelist.h minor_gc.h
hash.o: hash.c mlvalues.h config.h ../../config/m.h ../../config/s.h \
 misc.h memory.h gc.h major_gc.h freelist.h minor_gc.h str.h
intern.o: intern.c debugger.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h fail.h gc.h intext.h io.h memory.h \
 major_gc.h freelist.h minor_gc.h reverse.h
interncp.o: interncp.c debugger.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h fail.h gc.h intext.h io.h memory.h \
 major_gc.h freelist.h minor_gc.h reverse.h
interp.o: interp.c alloc.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h debugger.h fail.h instruct.h memory.h \
 gc.h major_gc.h freelist.h minor_gc.h prims.h signals.h stacks.h \
 str.h unalignd.h debugcom.h jumptbl.h
ints.o: ints.c alloc.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h fail.h memory.h gc.h major_gc.h \
 freelist.h minor_gc.h
io.o: io.c alloc.h misc.h config.h ../../config/m.h ../../config/s.h \
 mlvalues.h debugger.h fail.h io.h memory.h gc.h major_gc.h freelist.h \
 minor_gc.h signals.h sys.h
lexing.o: lexing.c mlvalues.h config.h ../../config/m.h \
 ../../config/s.h misc.h stacks.h memory.h gc.h major_gc.h freelist.h \
 minor_gc.h str.h
main.o: main.c alloc.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h debugcom.h debugger.h exec.h fail.h \
 gc_ctrl.h globals.h intext.h io.h stacks.h memory.h gc.h major_gc.h \
 freelist.h minor_gc.h sys.h ../version.h
major_gc.o: major_gc.c config.h ../../config/m.h ../../config/s.h \
 debugger.h misc.h mlvalues.h fail.h freelist.h gc.h gc_ctrl.h \
 globals.h major_gc.h roots.h
memory.o: memory.c debugger.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h fail.h freelist.h gc.h gc_ctrl.h \
 major_gc.h memory.h minor_gc.h
meta.o: meta.c alloc.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h globals.h major_gc.h freelist.h memory.h \
 gc.h minor_gc.h prims.h
minor_gc.o: minor_gc.c config.h ../../config/m.h ../../config/s.h \
 debugger.h misc.h mlvalues.h fail.h gc.h gc_ctrl.h major_gc.h \
 freelist.h memory.h minor_gc.h roots.h
misc.o: misc.c config.h ../../config/m.h ../../config/s.h debugger.h \
 misc.h mlvalues.h
parsing.o: parsing.c config.h ../../config/m.h ../../config/s.h \
 mlvalues.h misc.h memory.h gc.h major_gc.h freelist.h minor_gc.h \
 alloc.h
prims.o: prims.c mlvalues.h config.h ../../config/m.h ../../config/s.h \
 misc.h prims.h
roots.o: roots.c debugger.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h memory.h gc.h major_gc.h freelist.h \
 minor_gc.h stacks.h
signals.o: signals.c alloc.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h debugger.h signals.h stacks.h memory.h \
 gc.h major_gc.h freelist.h minor_gc.h
stacks.o: stacks.c config.h ../../config/m.h ../../config/s.h \
 debugger.h misc.h mlvalues.h debugcom.h fail.h stacks.h memory.h gc.h \
 major_gc.h freelist.h minor_gc.h
str.o: str.c alloc.h misc.h config.h ../../config/m.h ../../config/s.h \
 mlvalues.h debugger.h
sys.o: sys.c alloc.h misc.h config.h ../../config/m.h ../../config/s.h \
 mlvalues.h debugcom.h fail.h globals.h instruct.h signals.h stacks.h \
 memory.h gc.h major_gc.h freelist.h minor_gc.h sys.h
alloc.d.o: alloc.c alloc.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h debugger.h major_gc.h freelist.h memory.h \
 gc.h minor_gc.h stacks.h
compare.d.o: compare.c fail.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h memory.h gc.h major_gc.h freelist.h \
 minor_gc.h str.h
debugcom.d.o: debugcom.c misc.h config.h ../../config/m.h \
 ../../config/s.h debugger.h mlvalues.h fail.h stacks.h memory.h gc.h \
 major_gc.h freelist.h minor_gc.h io.h debugcom.h
debugger.d.o: debugger.c debugger.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h instruct.h memory.h gc.h major_gc.h \
 freelist.h minor_gc.h opnames.h stacks.h unalignd.h
extern.d.o: extern.c debugger.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h fail.h gc.h intext.h io.h memory.h \
 major_gc.h freelist.h minor_gc.h
externcp.d.o: externcp.c debugger.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h fail.h gc.h intext.h io.h memory.h \
 major_gc.h freelist.h minor_gc.h reverse.h
fail.d.o: fail.c alloc.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h fail.h memory.h gc.h major_gc.h \
 freelist.h minor_gc.h signals.h
fix_code.d.o: fix_code.c config.h ../../config/m.h ../../config/s.h \
 misc.h mlvalues.h instruct.h reverse.h
floats.d.o: floats.c alloc.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h debugger.h fail.h memory.h gc.h \
 major_gc.h freelist.h minor_gc.h
freelist.d.o: freelist.c config.h ../../config/m.h ../../config/s.h \
 debugger.h misc.h mlvalues.h freelist.h gc.h gc_ctrl.h major_gc.h
gc_ctrl.d.o: gc_ctrl.c alloc.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h debugger.h gc.h gc_ctrl.h major_gc.h \
 freelist.h minor_gc.h
hash.d.o: hash.c mlvalues.h config.h ../../config/m.h ../../config/s.h \
 misc.h memory.h gc.h major_gc.h freelist.h minor_gc.h str.h
intern.d.o: intern.c debugger.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h fail.h gc.h intext.h io.h memory.h \
 major_gc.h freelist.h minor_gc.h reverse.h
interncp.d.o: interncp.c debugger.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h fail.h gc.h intext.h io.h memory.h \
 major_gc.h freelist.h minor_gc.h reverse.h
interp.d.o: interp.c alloc.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h debugger.h fail.h instruct.h memory.h \
 gc.h major_gc.h freelist.h minor_gc.h prims.h signals.h stacks.h \
 str.h unalignd.h debugcom.h
ints.d.o: ints.c alloc.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h fail.h memory.h gc.h major_gc.h \
 freelist.h minor_gc.h
io.d.o: io.c alloc.h misc.h config.h ../../config/m.h ../../config/s.h \
 mlvalues.h debugger.h fail.h io.h memory.h gc.h major_gc.h freelist.h \
 minor_gc.h signals.h sys.h
lexing.d.o: lexing.c mlvalues.h config.h ../../config/m.h \
 ../../config/s.h misc.h stacks.h memory.h gc.h major_gc.h freelist.h \
 minor_gc.h str.h
main.d.o: main.c alloc.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h debugcom.h debugger.h exec.h fail.h \
 gc_ctrl.h globals.h intext.h io.h stacks.h memory.h gc.h major_gc.h \
 freelist.h minor_gc.h sys.h ../version.h
major_gc.d.o: major_gc.c config.h ../../config/m.h ../../config/s.h \
 debugger.h misc.h mlvalues.h fail.h freelist.h gc.h gc_ctrl.h \
 globals.h major_gc.h roots.h
memory.d.o: memory.c debugger.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h fail.h freelist.h gc.h gc_ctrl.h \
 major_gc.h memory.h minor_gc.h
meta.d.o: meta.c alloc.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h globals.h major_gc.h freelist.h memory.h \
 gc.h minor_gc.h prims.h
minor_gc.d.o: minor_gc.c config.h ../../config/m.h ../../config/s.h \
 debugger.h misc.h mlvalues.h fail.h gc.h gc_ctrl.h major_gc.h \
 freelist.h memory.h minor_gc.h roots.h
misc.d.o: misc.c config.h ../../config/m.h ../../config/s.h debugger.h \
 misc.h mlvalues.h
parsing.d.o: parsing.c config.h ../../config/m.h ../../config/s.h \
 mlvalues.h misc.h memory.h gc.h major_gc.h freelist.h minor_gc.h \
 alloc.h
prims.d.o: prims.c mlvalues.h config.h ../../config/m.h ../../config/s.h \
 misc.h prims.h
roots.d.o: roots.c debugger.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h memory.h gc.h major_gc.h freelist.h \
 minor_gc.h stacks.h
signals.d.o: signals.c alloc.h misc.h config.h ../../config/m.h \
 ../../config/s.h mlvalues.h debugger.h signals.h stacks.h memory.h \
 gc.h major_gc.h freelist.h minor_gc.h
stacks.d.o: stacks.c config.h ../../config/m.h ../../config/s.h \
 debugger.h misc.h mlvalues.h debugcom.h fail.h stacks.h memory.h gc.h \
 major_gc.h freelist.h minor_gc.h
str.d.o: str.c alloc.h misc.h config.h ../../config/m.h ../../config/s.h \
 mlvalues.h debugger.h
sys.d.o: sys.c alloc.h misc.h config.h ../../config/m.h ../../config/s.h \
 mlvalues.h debugcom.h fail.h globals.h instruct.h signals.h stacks.h \
 memory.h gc.h major_gc.h freelist.h minor_gc.h sys.h
