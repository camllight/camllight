# Makefile for the Caml Light runtime system

# These options are overriden when called from ../Makefile.
CC=gcc
OPTS=-DANSI
CFLAGS=-O $(OPTS)

# This is the list of libraries that have to be linked with the runtime
# system. On most machines, all you need is the math library "-lm" 
LIBS=-lm

# Test to see whether ranlib exists on the machine
RANLIBTEST=test -f /usr/bin/ranlib -o -f /bin/ranlib

# How to invoke ranlib
RANLIB=ranlib

#========== End of configuration section


OBJS=interp.o equal.o ints.o floats.o str.o io.o intext.o \
        hash.o sys.o meta.o freelist.o major_gc.o minor_gc.o \
        memory.o misc.o alloc.o fail.o main.o stacks.o \
        fix_code.o roots.o version.o parsing.o lexing.o signals.o

DOBJS=debugger.o

PRIMS=interp.c equal.c ints.c floats.c str.c \
        io.c intext.c hash.c sys.c meta.c parsing.c lexing.c

all: camlrun

camlrun: libcaml.a prims.o
	$(CC) $(CFLAGS) -o camlrun prims.o libcaml.a $(LIBS)

camlrund: opnames.h libcaml.a prims.o $(DOBJS)
	$(CC) $(CFLAGS) -o camlrund prims.o $(DOBJS) libcaml.a $(LIBS)

debug:
	make clean
	make CFLAGS="-g $(OPTS) -DDEBUG" camlrund

libcaml.a: $(OBJS)
	rm -f libcaml.a
	ar rc libcaml.a $(OBJS)
	if $(RANLIBTEST); then $(RANLIB) libcaml.a; else true; fi

clean:
	rm -f camlrun camlrund *.o *.a
	rm -f primitives prims.c opnames.h jumptbl.h

install:
	cp camlrun $(BINDIR)
	strip $(BINDIR)/camlrun
	cp libcaml.a $(LIBDIR)
	if $(RANLIBTEST); then cd $(LIBDIR); $(RANLIB) libcaml.a; else true; fi
	for i in mlvalues.h alloc.h misc.h; do \
	sed -e 's|#include "\([^"]*\)"|#include "'$(LIBDIR)'/\1"|' \
		$$i > $(LIBDIR)/$$i; \
 done
	sed -e '/#include ".*\/m.h/r ../../config/m.h' \
            -e '/#include ".*\/s.h/r ../../config/s.h' \
            -e '/#include "/d' config.h > $(LIBDIR)/config.h
	sed -e '/#include ".*gc\.h"/d' \
            -e 's|#include "\([^"]*\)"|#include "'$(LIBDIR)'/\1"|' \
	    -e '/#define Alloc_small/,/^}/d' \
	    -e '/Modify/,/^}/d' memory.h > $(LIBDIR)/memory.h

primitives : $(PRIMS)
	sed -n -e '/\/\* ML \*\//s/.* \([a-z0-9_]*\)(.*/\1/p' \
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
	sed -n -e 's/^  /  \&\&lbl_/p' -e '/^}/q' instruct.h > jumptbl.h

depend : prims.c opnames.h jumptbl.h
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak;	 \
         gcc -MM $(OPTS) *.c) > Makefile
	rm Makefile.bak

### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
alloc.o : alloc.c alloc.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  debugger.h major_gc.h freelist.h misc.h memory.h gc.h minor_gc.h stacks.h 
debugger.o : debugger.c debugger.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  instruct.h memory.h gc.h major_gc.h freelist.h misc.h minor_gc.h opnames.h stacks.h \
  unalignd.h 
equal.o : equal.c fail.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  memory.h gc.h major_gc.h freelist.h misc.h minor_gc.h str.h 
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
lexing.o : lexing.c mlvalues.h config.h ../../config/m.h ../../config/s.h stacks.h \
  memory.h gc.h major_gc.h freelist.h misc.h minor_gc.h str.h 
main.o : main.c alloc.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  exec.h fail.h gc.h globals.h intext.h io.h misc.h stacks.h memory.h major_gc.h \
  freelist.h minor_gc.h sys.h 
major_gc.o : major_gc.c config.h ../../config/m.h ../../config/s.h debugger.h \
  mlvalues.h fail.h freelist.h gc.h globals.h major_gc.h misc.h roots.h 
memory.o : memory.c debugger.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  fail.h freelist.h gc.h major_gc.h misc.h memory.h minor_gc.h 
meta.o : meta.c alloc.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  globals.h major_gc.h freelist.h misc.h memory.h gc.h minor_gc.h prims.h 
minor_gc.o : minor_gc.c config.h ../../config/m.h ../../config/s.h debugger.h \
  mlvalues.h fail.h gc.h major_gc.h freelist.h misc.h memory.h minor_gc.h roots.h 
misc.o : misc.c config.h ../../config/m.h ../../config/s.h debugger.h mlvalues.h \
  misc.h 
parsing.o : parsing.c config.h ../../config/m.h ../../config/s.h mlvalues.h \
  memory.h gc.h major_gc.h freelist.h misc.h minor_gc.h 
prims.o : prims.c mlvalues.h config.h ../../config/m.h ../../config/s.h prims.h 
roots.o : roots.c debugger.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  memory.h gc.h major_gc.h freelist.h misc.h minor_gc.h stacks.h 
signals.o : signals.c mlvalues.h config.h ../../config/m.h ../../config/s.h \
  signals.h alloc.h stacks.h memory.h gc.h major_gc.h freelist.h misc.h minor_gc.h 
stacks.o : stacks.c config.h ../../config/m.h ../../config/s.h debugger.h mlvalues.h \
  fail.h misc.h stacks.h memory.h gc.h major_gc.h freelist.h minor_gc.h 
str.o : str.c alloc.h mlvalues.h config.h ../../config/m.h ../../config/s.h \
  debugger.h 
sys.o : sys.c config.h ../../config/m.h ../../config/s.h alloc.h mlvalues.h \
  fail.h globals.h instruct.h signals.h stacks.h memory.h gc.h major_gc.h freelist.h \
  misc.h minor_gc.h 
version.o : version.c 
