# Where the caml-tex command resides
BINDIR=/usr/local/bin

# Where the TeX input files reside
INPUTDIR=/usr/local/lib/tex/inputs

# In which section of the manual the man page must be installed
MANEXT=1

# Where the man page resides
MANDIR=/usr/man/man$(MANEXT)

install:
	cp caml-tex $(BINDIR)/caml-tex
	cp caml.sty caml-sl.sty $(INPUTDIR)
	cp caml-tex.1 $(MANDIR)/caml-tex.$(MANEXT)

caml-tex.doc: caml-tex.1
	nroff -man caml-tex.1 | sed -e 's/_//g' > caml-tex.doc

clean:
	rm -f *~ #*#
