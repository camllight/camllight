CAMLCOMP=../camlrun ../camlcomp -stdlib ../lib
CAMLLINK=../camlrun ../camllink -stdlib ../lib
CC=cc

all: camlc camllight camlmktop camlexec testprog 

# header cannot be generated until camlrun is installed in $(BINDIR)...
# Also, "make install" is done with root permissions, meaning that we don't
# have write permission in the current directory if NFS-mounted...

install:
	(echo "#!$(BINDIR)/camlrun"; \
	 echo "exit 2"; \
	 cat testprog) > /tmp/testscr
	chmod a+x /tmp/testscr
	sh -c 'if sh -c /tmp/testscr 2>/dev/null; \
               then echo "#!$(BINDIR)/camlrun" > $(LIBDIR)/header; \
               else cp camlexec $(LIBDIR)/header; \
               fi'
	rm -f /tmp/testscr
	for script in camlc camllight camlmktop; do \
	  cp $$script $(BINDIR)/$$script; \
	  chmod a+x $(BINDIR)/$$script; \
	done

camlc: camlc.tpl
	sed -e "s|LIBDIR|$(LIBDIR)|" -e "s|CC|$(CC)|" camlc.tpl > camlc

camllight: camllight.tpl
	sed -e "s|LIBDIR|$(LIBDIR)|" -e "s|CC|$(CC)|" camllight.tpl > camllight

camlmktop: camlmktop.tpl
	sed -e "s|LIBDIR|$(LIBDIR)|" -e "s|CC|$(CC)|" camlmktop.tpl > camlmktop

testprog: testprog.ml
	$(CAMLCOMP) testprog.ml
	$(CAMLLINK) -o testprog stdlib.zo testprog.zo

camlexec: camlexec.c
	$(CC) $(OPTS) -o camlexec camlexec.c
	strip camlexec

clean:
	rm -f camlexec camlc camllight camlmktop testprog testprog.z[io]

depend:
