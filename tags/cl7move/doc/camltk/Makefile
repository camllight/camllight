LATEX=latex
CAMLTKDIR=../lib
TKSUPDIR=../libsupport

# THIS MAKEFILE IS BOGUS, DON'T RELY ON DEPENDANCIES

TEXFILES=camltk.tex user.tex impl.tex reference.tex bnf.tex \
         inclmod.tex

.SUFFIXES: .tex .bib .bbl


all: camltk.dvi html

camltk.dvi: $(TEXFILES) camltk.bbl
	$(LATEX) camltk.tex

camltk.bbl: tk.bib
	bibtex camltk

camltk.aux: camltk.tex
	$(LATEX) camltk.tex

clean: 
	rm -f camltk.dvi camltk.log camltk.blg tk.mli inclmod.tex

scratch: clean
	rm -f camltk.blg camltk.toc inclmod.tex *.aux

inclmod.tex: $(CAMLTKDIR)/modules
	./makemodinc > inclmod.tex


## The WWW Documentation

SUPPORT=$(TKSUPDIR)/support.mli $(TKSUPDIR)/textvariable.mli

GENERATED=`sed -e 's/WIDGETOBJS=//' -e 's,\([a-z_]*\)\.zo,\$(CAMLTKDIR)/\1.mli,g' \
                      $(CAMLTKDIR)/modules`

tk.mli: tkbuiltin.mli $(CAMLTKDIR)/tkgen.mli
	cat tkbuiltin.mli $(CAMLTKDIR)/tkgen.mli > tk.mli

TKMODULE=tk.mli

INTERFACES=\
   '%Support' $(SUPPORT) \
   '%Widgets' $(GENERATED) \
   '%Tk' $(TKMODULE)


mlifiles: tk.mli
	perl format-mli-files $(INTERFACES)

html: mlifiles
	latex2html -split 2  -dont_include reference camltk.tex

cleanhtml:
	cd camltk; rm -f node*.html camltk.html images.* labels.*
