# Makefile for the Caml Light runtime system

# These options are overriden when called from ../Makefile.
CC=cc
OPTS=#-fno-defer-pop
CFLAGS=-O $(OPTS) -I..

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
        fix_code.o roots.o parsing.o lexing.o signals.o debugcom.o gc_ctrl.o

DOBJS=$(OBJS:.o=.d.o) debugger.d.o

PRIMS=interp.c equal.c ints.c floats.c str.c gc_ctrl.c \
        io.c intext.c hash.c sys.c meta.c parsing.c lexing.c

all: camlrun libcaml.a

camlrun: $(OBJS) prims.o
	$(CC) $(CFLAGS) -o camlrun prims.o $(OBJS) $(LIBS)

camlrund: $(DOBJS) prims.o
	$(CC) -g $(OPTS) -o camlrund prims.o $(DOBJS) $(LIBS)

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
         gcc -MM $(OPTS) *.c; \
         gcc -MM $(OPTS) -DDEBUG *.c | sed -e 's/\.o/.d.o/') > Makefile
	rm Makefile.bak

### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
