CAMLC=camlc
CAMLCOMP=$(CAMLC) -c
CPP=/lib/cpp -P -Dunix
COMPFLAGS=-W -O fast -g -I ../../libunix
LINKFLAGS=-g
CAMLYACC=camlyacc -v
CAMLLEX=camllex
CAMLDEP=../../../src/tools/camldep
RANLIBTEST=test -f /usr/bin/ranlib -o -f /bin/ranlib
RANLIB=ranlib

LIBDIR=/usr/local/lib/caml-light

X11_INCLUDES=/usr/include
TCLINCLDIR=/usr/local/lib/tcl7.4/include
TKINCLDIR=/usr/local/lib/tk4.0/include

all: catchexc.zo hashtblc.zo support.zo protocol.zo textvariable.zo optionmenu.zo \
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
	cp dialog.zi $(LIBDIR)
	cp dialog.zix $(LIBDIR)
	cp protocol.zi $(LIBDIR)
	cp protocol.zix $(LIBDIR)
	cp libcamltk4.a $(LIBDIR)
	chmod 644 $(LIBDIR)/*.zi* $(LIBDIR)/libcamltk4.a

clean : 
	rm -f *.z* *.o *.a

.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo .mlp

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<


depend: 
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
