# Makefile for the Caml Light librarian.

CAMLCOMP=../camlrun ../camlcomp
CAMLLINK=../camlrun ../camllink
INCLUDES=-stdlib ../lib -I ../compiler -I ../linker
COMPFLAGS=-O fast $(INCLUDES)
LINKFLAGS=-g $(INCLUDES)

EXTERNOBJS=config.zo misc.zo readword.zo 

OBJS=librar.zo version.zo main.zo

all: camllibr

camllibr: $(OBJS)
	$(CAMLLINK) $(LINKFLAGS) -o camllibr stdlib.zo $(EXTERNOBJS) $(OBJS)

clean:
	rm -f *.zi *.zo camllibr

install:
	cat $(LIBDIR)/header camllibr > $(BINDIR)/camllibr
	chmod a+x $(BINDIR)/camllibr

.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

depend:
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
         ../tools/camldep -I ../compiler -I ../linker *.ml) > Makefile
	rm Makefile.bak

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
librar.zo: ../compiler/emit_phr.zo ../compiler/misc.zo 
main.zo: librar.zo version.zo ../compiler/misc.zo ../linker/readword.zo \
    ../compiler/config.zi 
