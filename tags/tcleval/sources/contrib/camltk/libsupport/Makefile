# Set environment variable PROF to -p to obtained profiled version
PROFILING=${CAMLPROF}
CAMLC=camlc
CAMLCOMP=$(CAMLC) -c $(PROFILING)
CPP=/lib/cpp -P -Dunix
COMPFLAGS=-W -O fast -g -I ../../libunix
LINKFLAGS=-g $(PROFILING)
CAMLYACC=camlyacc -v
CAMLLEX=camllex
CAMLDEP=../../../src/tools/camldep

all: support.zo textvariable.zo camltk.zi libcamltk.a protocol1.zo

protocol1.zo: protocol1.ml
	rm -f protocol.ml
	cp -p protocol1.ml protocol.ml
	$(CAMLCOMP) $(COMPFLAGS) protocol.ml
	mv protocol.zo protocol1.zo

# Not supported anymore
# protocol2.zo: protocol2.ml
#	rm -f protocol.ml
#	cp -p protocol2.ml protocol.ml
#	$(CAMLCOMP) $(COMPFLAGS) protocol.ml
#	mv protocol.zo protocol2.zo


camltk.o : camltk.c
	$(CC) -g -c -I../../../src/runtime \
		 -I$(X11_INCLUDES) \
		 -I$(TCLINCLDIR) \
		 -I$(TKINCLDIR) \
		 camltk.c

libcamltk.a : camltk.o
	rm -f libcamltk.a
	ar rc libcamltk.a camltk.o
	if $(RANLIBTEST); then $(RANLIB) libcamltk.a; else true; fi

install: libcamltk.a support.zi textvariable.zi protocol.zi
	if test -d $(LIBDIR); then : ; else mkdir $(LIBDIR); fi
	cp support.zi $(LIBDIR)
	cp support.zix $(LIBDIR)
	cp textvariable.zi $(LIBDIR)
	cp textvariable.zix $(LIBDIR)
	cp protocol.zi $(LIBDIR)
	cp libcamltk.a $(LIBDIR)
	chmod 644 $(LIBDIR)/*.zi $(LIBDIR)/libcamltk.a

clean : 
	rm -f *.z* *.o *.a protocol.ml

.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo .mlp

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<


depend: 
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
	 $(CAMLDEP) *.mli *.ml) > Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
protocol.zi: support.zi 
protocol1.zo: support.zi camltk.zi 
protocol2.zo: support.zi 
support.zo: support.zi 
textvariable.zo: textvariable.zi protocol.zi support.zi 
