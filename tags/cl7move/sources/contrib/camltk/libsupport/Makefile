## Configuration
include ../Makefile.config

all: support.zo textvariable.zo camltk.zi camltk.o protocol1.zo protocol2.zo

protocol1.zo: protocol1.ml
	rm -f protocol.ml
	cp -p protocol1.ml protocol.ml
	$(CAMLCOMP) $(COMPFLAGS) protocol.ml
	mv protocol.zo protocol1.zo

protocol2.zo: protocol2.ml
	rm -f protocol.ml
	cp -p protocol2.ml protocol.ml
	$(CAMLCOMP) $(COMPFLAGS) protocol.ml
	mv protocol.zo protocol2.zo


camltk.o : camltk.c
	$(CC) -g -c -I/usr/local/lib/caml-light \
		 -I$(TCLINCLDIR) \
		 -I$(TKINCLDIR) \
		 camltk.c

install: camltk.o support.zi textvariable.zi protocol.zi
	[ -d $(INSTALLDIR) ] || mkdir $(INSTALLDIR)
	cp -p support.zi $(INSTALLDIR)
	cp -p textvariable.zi $(INSTALLDIR)
	cp -p protocol.zi $(INSTALLDIR)
	cp -p camltk.o $(INSTALLDIR)
	chmod 644 $(INSTALLDIR)/*.zi $(INSTALLDIR)/camltk.o

clean : 
	rm -f *.z* *.o protocol.ml

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
