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
X11_INCLUDES=/usr/include
TCLINCLDIR=/usr/local/lib/tcl/include
TKINCLDIR=/usr/local/lib/tk/include
RANLIBTEST=test -f /usr/bin/ranlib -o -f /bin/ranlib
RANLIB=ranlib

all: support.zo textvariable.zo camltk.zi libcamltk.a protocol.zo

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
	rm -f *.z* *.o *.a

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
camltk.zi: protocol.zi 
protocol.zi: support.zi 
textvariable.zi: protocol.zi 
mainloop.zo: protocol.zi 
protocol.zo: protocol.zi support.zi camltk.zi 
support.zo: support.zi protocol.zi 
textvariable.zo: textvariable.zi protocol.zi support.zi 
