# Makefile for the Caml Light librarian.

CAMLCOMP=../camlrun ../camlcomp
CAMLLINK=../camlrun ../camllink
INCLUDES=-stdlib ../lib -I ../compiler -I ../linker
COMPFLAGS=-W -O fast $(INCLUDES)
LINKFLAGS=-g $(INCLUDES)
CPP=/lib/cpp -P -Dunix

EXTERNOBJS=config.zo misc.zo interntl.zo readword.zo

OBJS=librar.zo version.zo main.zo

GENSOURCES=version.ml

all: camllibr

camllibr: $(OBJS)
	$(CAMLLINK) $(LINKFLAGS) -o camllibr stdlib.zo $(EXTERNOBJS) $(OBJS)

clean:
	rm -f *.zi *.zo camllibr
	rm -f $(GENSOURCES)

install:
	cat $(LIBDIR)/header camllibr > $(BINDIR)/camllibr
	chmod a+x $(BINDIR)/camllibr

.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo .mlp

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.mlp.ml:
	@rm -f $@
	$(CPP) $< > $@
	@chmod a-w $@

depend: $(GENSOURCES)
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
         ../tools/camldep -I ../compiler -I ../linker *.ml) > Makefile
	rm Makefile.bak

version.ml: version.mlp ../version.h

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
librar.zo: ../compiler/emit_phr.zo ../compiler/misc.zo \
    ../compiler/interntl.zo 
main.zo: librar.zo version.zo ../compiler/misc.zo ../linker/readword.zo \
    ../compiler/interntl.zo ../compiler/config.zi 
version.zo: ../compiler/interntl.zo 
