# Makefile for the Caml Light linker.

CAMLCOMP=../camlrun ../camlcomp
CAMLLINK=../camlrun ../camllink
INCLUDES=-stdlib ../lib -I ../compiler
COMPFLAGS=-O fast $(INCLUDES)
LINKFLAGS=-g $(INCLUDES)

EXTERNOBJS=config.zo misc.zo opcodes.zo

OBJS=predef.zo prim_c.zo symtable.zo patch.zo tr_const.zo link.zo \
    readword.zo version.zo main.zo

all: camllink

camllink: $(OBJS)
	$(CAMLLINK) $(LINKFLAGS) -o camllink stdlib.zo $(EXTERNOBJS) $(OBJS)

clean:
	rm -f *.zi *.zo camllink
	rm -f predef.ml prim_c.ml

install:
	cp camllink $(LIBDIR)/camllink

prim_c.ml : ../runtime/primitives
	(echo 'let primitives_table = [|'; \
	 sed -e 's/.*/  "&";/' -e '$$s/;$$//' ../runtime/primitives; \
	 echo '|];;') > prim_c.ml

predef.ml : ../runtime/globals.h ../runtime/fail.h
	(echo '#open "const";;'; \
         echo 'let predef_variables = ['; \
	 sed -n -e 's|.*/\* \(".*"\), *\(".*"\) \*/$$|{qual=\1; id=\2};|p' \
                ../runtime/globals.h \
           | sed -e '$$s|;$$||'; \
         echo '];;'; \
         echo 'let predef_exn = ['; \
         sed -n -e 's|.*/\* \(".*"\), *\(".*"\), *\([0-9]*\) \*/$$|({qual=\1; id=\2}, \3);|p' \
                ../runtime/fail.h \
           | sed -e '$$s|;$$||'; \
         echo '];;') > predef.ml

.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

depend: prim_c.ml predef.ml
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
         ../tools/camldep -I ../compiler *.ml) > Makefile
	rm Makefile.bak

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
link.zo: ../compiler/reloc.zo tr_const.zo ../compiler/config.zi \
    ../compiler/const.zo ../compiler/emit_phr.zo ../compiler/misc.zo \
    symtable.zo patch.zo ../compiler/instruct.zo ../compiler/opcodes.zo 
main.zo: link.zo ../compiler/config.zi ../compiler/misc.zo version.zo \
    symtable.zo readword.zo 
patch.zo: symtable.zo ../compiler/reloc.zo 
predef.zo: ../compiler/const.zo 
symtable.zo: predef.zo ../compiler/misc.zo prim_c.zo ../compiler/const.zo 
tr_const.zo: symtable.zo ../compiler/const.zo 
