MANDIR=/usr/man/man1
BINDIR=/usr/local/bin

CPP=/lib/cpp -P -Dunix
CAMLCOMP=camlc -c -g

COMPFLAGS=-I ../../src/compiler
LINKFLAGS=-g -I ../../src/compiler

COMPILEROBJS=misc.zo config.zo
OBJS=tsort.zo main.zo


all: camllorder
	
camllorder: $(OBJS)
	camlc $(LINKFLAGS) -o camllorder $(COMPILEROBJS) $(OBJS)

clean:
	rm -f *.z[oi] camllorder

scratch:
	rm -f *.z[oi] camllorder
	rm -f config.ml
	rm -f camllorder.tar.Z

install:
	cp camllorder $(BINDIR)
	cp camllorder.1 $(MANDIR)

distrib:
	make scratch
	cd ..; \
	echo lorder/test >/tmp/x$$$$; \
	find lorder -type d -name RCS -print >>/tmp/x$$$$; \
	find lorder -name '.*~' -print >>/tmp/x$$$$; \
	find lorder -name '*~' -print >>/tmp/x$$$$; \
	tar cvXf /tmp/x$$$$ /tmp/lorder.tar lorder; \
	rm -f /tmp/x$$$$
	compress /tmp/lorder.tar
	@echo "Distrib is in /tmp/lorder.tar.Z"
	

.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo .mlp

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.mlp.ml:
	$(CPP) $< > $@

depend: 
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
         ../../src/tools/camldep *.mli *.ml) > Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
main.zo: tsort.zo 
