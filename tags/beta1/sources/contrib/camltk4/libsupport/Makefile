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

all: hashtblc.zo support.zo protocol.zo textvariable.zo optionmenu.zo \
     camltk.zi libcamltk4.a

camltk.o : camltk.c
	$(CC) -g -c -I../../../src/runtime \
		 -I$(X11_INCLUDES) \
		 -I$(TCLINCLDIR) \
		 -I$(TKINCLDIR) \
		 camltk.c

libcamltk4.a : camltk.o
	rm -f libcamltk4.a
	ar rc libcamltk4.a camltk.o
	if $(RANLIBTEST); then $(RANLIB) libcamltk4.a; else true; fi

install: libcamltk4.a support.zi textvariable.zi protocol.zi
	if test -d $(LIBDIR); then : ; else mkdir $(LIBDIR); fi
	cp support.zi $(LIBDIR)
	cp support.zix $(LIBDIR)
	cp textvariable.zi $(LIBDIR)
	cp textvariable.zix $(LIBDIR)
	cp optionmenu.zi $(LIBDIR)
	cp optionmenu.zix $(LIBDIR)
	cp protocol.zi $(LIBDIR)
	cp libcamltk4.a $(LIBDIR)
	chmod 644 $(LIBDIR)/*.zi $(LIBDIR)/libcamltk4.a

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
optionmenu.zi: support.zi textvariable.zi 
protocol.zi: support.zi 
textvariable.zi: protocol.zi 
hashtblc.zo: hashtblc.zi 
mainloop.zo: protocol.zi 
optionmenu.zo: optionmenu.zi protocol.zi support.zi textvariable.zi 
protocol.zo: protocol.zi hashtblc.zi support.zi camltk.zi 
support.zo: support.zi 
textvariable.zo: textvariable.zi protocol.zi 
