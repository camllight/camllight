### System dependent configuration section

COMPLIBDIR=../../../src/compiler
INSTALLDIR=/usr/local/bin
MANDIR=/usr/local/man/manl
ELISPDIR=/usr/local/lib/emacs/site-lisp

### End of system dependent configuration section

PACKAGE=camlsearch.tar.Z

all:
	(cd source; make "COMPLIBDIR=$(COMPLIBDIR)")

install:
	(cd source; make install "INSTALLDIR=$(INSTALLDIR)" "COMPLIBDIR=$(COMPLIBDIR)")
	(cd doc; make install "MANDIR=$(MANDIR)")
	(cd emacs-lisp; make install "ELISPDIR=$(ELISPDIR)")

distrib: clean
	(cd ..; rm -f $(PACKAGE); \
	 tar -cvf - camlsearch | compress -c > $(PACKAGE))

clean:
	(cd source; make clean)
	-rm *~

