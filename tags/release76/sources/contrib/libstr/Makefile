# Makefile for the strings library

# These options are overriden when called from ../Makefile
CC=cc
OPTS=

# Test to see whether ranlib exists on the machine
RANLIBTEST=test -f /usr/bin/ranlib -o -f /bin/ranlib

# How to invoke ranlib
RANLIB=ranlib

# Compilation options
CFLAGS=-O -I$(REGEXLIB) -I../../src/runtime
CAMLC=camlc
CAMLCOMP=$(CAMLC) -c
COMPFLAGS=-W
REGEXLIB=regex-0.12
COBJS=strstubs.o $(REGEXLIB)/regex.o

all: libstr.a str.zo camlstr

libstr.a: $(COBJS)
	rm -f libstr.a
	ar rc libstr.a $(COBJS)
	if test -f /bin/ranlib -o -f /usr/bin/ranlib; then ranlib libstr.a; fi

camlstr: libstr.a str.zo
	camlmktop -o camlstr -custom str.zo libstr.a

$(REGEXLIB)/regex.o: $(REGEXLIB)/regex.c $(REGEXLIB)/regex.h
	cd $(REGEXLIB); CC="$(CC)" sh configure; make

clean:
	rm -f libstr.a *.o *.zo *.zi camlstr
	rm -f *.dvi *.log *.aux
	cd $(REGEXLIB); if test -f Makefile; then make distclean; else exit 0; fi

install:
	cp libstr.a $(LIBDIR)/libstr.a
	if $(RANLIBTEST); then cd $(LIBDIR); $(RANLIB) libstr.a; else true; fi
	cp str.zo str.zi $(LIBDIR)
	cp camlstr $(LIBDIR)

.SUFFIXES: .ml .mli .zo .zi .tex

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

depend:
	mv Makefile Makefile.bak; \
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
         gcc -MM $(CFLAGS) *.c; \
         ../../src/tools/camldep *.mli *.ml) > Makefile
	rm -f Makefile.bak

### DO NOT DELETE THIS LINE
strstubs.o: strstubs.c
istr.zi: str.zi 
str.zo: str.zi istr.zi 
