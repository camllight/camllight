# Overriden when called from ../Makefile
BINDIR=/usr/local/bin
TEXINPUTDIR=/usr/local/tex/inputs
MANEXT=1
MANDIR=/usr/local/man/man$(MANEXT)

all:

install:
	- cp caml-tex $(BINDIR)/caml-tex
	- cp caml.sty caml-sl.sty $(TEXINPUTDIR)
	- cp caml-tex.1 $(MANDIR)/caml-tex.$(MANEXT)

caml-tex.doc: caml-tex.1
	nroff -man caml-tex.1 | sed -e 's/_//g' > caml-tex.doc

clean:
	rm -f *~ #*#
