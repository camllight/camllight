### System dependent configuration section

COMPLIBDIR=../../../src/compiler
UNIXLIBDIR=../../libunix

# Overriden when called from ../Makefile
BINDIR=/usr/local/bin
MANEXT=l
MANDIR=/usr/local/man/man$(MANEXT)
EMACSLISPDIR=/usr/local/lib/emacs/site-lisp

### End of system dependent configuration section

PACKAGE=camlsearch.tar.Z

all:
	(cd source; \
         $(MAKE) "COMPLIBDIR=$(COMPLIBDIR)" "UNIXLIBDIR=$(UNIXLIBDIR)")

install:
	(cd source; $(MAKE) install "INSTALLDIR=$(BINDIR)" \
                "COMPLIBDIR=$(COMPLIBDIR)" "UNIXLIBDIR=$(UNIXLIBDIR)")
	(cd doc; $(MAKE) install "MANEXT=$(MANEXT)" "MANDIR=$(MANDIR)")
	(cd emacs-lisp; $(MAKE) install "ELISPDIR=$(EMACSLISPDIR)")

distrib: clean
	(cd ..; rm -f $(PACKAGE); \
	 tar -cvf - camlsearch | compress -c > $(PACKAGE))

clean:
	(cd source; $(MAKE) clean)
	rm -f *~

depend:
	(cd source; $(MAKE) depend)
