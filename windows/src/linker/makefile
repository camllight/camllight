# Makefile for the Caml Light linker.

CAMLCOMP=..\camlrun ..\camlcomp
CAMLRUN=..\camlrun
CAMLLINK=..\camlrun ..\camllink
CAMLLIBR=..\camlrun ..\camllibr
CAMLLEX=..\camlrun ..\camllex
CAMLYACC=..\camlyacc -s
CPP=cl -EP -Dmsdos -Uunix
PERL=perl
COMPFLAGS=-stdlib ../lib -W -O fast $(INCLUDES)
LINKFLAGS=-stdlib ../lib -g $(INCLUDES)

OUTPUT=camllink

INCLUDES=-I ../compiler

EXTERNOBJS=config.zo misc.zo interntl.zo opcodes.zo

OBJS=predef.zo prim_c.zo symtable.zo patch.zo tr_const.zo link.zo \
    readword.zo version.zo main.zo

GENSOURCES=version.ml prim_c.ml predef.ml

all: camllink

camllink: $(OBJS)
	$(CAMLLINK) $(LINKFLAGS) -o camllink @<<
stdlib.zo $(EXTERNOBJS) $(OBJS)
<<

install:
	copy camllink $(LIBDIR)

prim_c.ml : ../runtime/prims
	$(PERL) ../tools/mkprimc ../runtime/prims > prim_c.ml

predef.ml : ../runtime/globals.h ../runtime/fail.h
	$(PERL) ../tools/mkpredef ../runtime/globals.h ../runtime/fail.h > predef.ml

!include "../tools/make-ml.inc"

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
link.zo: tr_const.zo symtable.zo patch.zo 
main.zo: link.zo version.zo symtable.zo readword.zo 
patch.zo: symtable.zo 
symtable.zo: predef.zo prim_c.zo 
tr_const.zo: symtable.zo 
