# Makefile for the Caml Light linker.

OUTPUT=camllink

INCLUDES=-I ../compiler

EXTERNOBJS=config.zo misc.zo opcodes.zo

OBJS=predef.zo prim_c.zo symtable.zo patch.zo tr_const.zo link.zo \
    readword.zo version.zo main.zo

GENSOURCES=version.ml prim_c.ml predef.ml

all: camllink

camllink: $(OBJS)
	$(CAMLLINK) $(LINKFLAGS) -o camllink stdlib.zo @&&!
$(EXTERNOBJS) $(OBJS)
!

install:
	copy camllink $(LIBDIR)

prim_c.ml : ../runtime/prims
	perl ../tools/mkprimc ../runtime/prims > prim_c.ml

predef.ml : ../runtime/globals.h ../runtime/fail.h
	perl ../tools/mkpredef ../runtime/globals.h ../runtime/fail.h > predef.ml

!include "../tools/make-ml.inc"

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
