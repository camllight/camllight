LATEX=latex
CAMLTKDIR=../lib
TKSUPDIR=../libsupport

# THIS MAKEFILE IS BOGUS, DON'T RELY ON DEPENDANCIES

TEXFILES=camltk.tex user.tex impl.tex reference.tex bnf.tex
GENFILES=inclmod.tex bindtypes.mli tktypes.mli

.SUFFIXES: .tex .bib .bbl


all: camltk.dvi html

camltk.dvi: $(TEXFILES) $(GENFILES) camltk.bbl
	$(LATEX) camltk.tex

camltk.bbl: tk.bib
	bibtex camltk

camltk.aux: camltk.tex
	$(LATEX) camltk.tex

inclmod.tex: $(CAMLTKDIR)/modules makemodinc
	./makemodinc > inclmod.tex


bindtypes.mli: $(TKSUPDIR)/builtin_bind.ml
	rm -f bindtypes.mli
	(echo '\begin{verbatim}'; \
	 sed -n -e '/^type/,/^;;$$/p' $(TKSUPDIR)/builtin_bind.ml; \
	 echo '\end{verbatim}') > bindtypes.mli

tktypes.mli: $(CAMLTKDIR)/tk.ml
	rm -f tktypes.mli
	(echo '\begin{verbatim}'; \
	 sed -n -e '/^type/,/^;;$$/p' $(CAMLTKDIR)/tk.ml; \
	 echo '\end{verbatim}') | expand > tktypes.mli
	

## The WWW Documentation

SUPPORT=$(TKSUPDIR)/support.mli $(TKSUPDIR)/textvariable.mli

GENERATED=`sed -e 's/WIDGETOBJS=//' -e 's,\([a-z_]*\)\.zo,\$(CAMLTKDIR)/\1.mli,g' \
                      $(CAMLTKDIR)/modules`

tk.mli: tkbuiltin.mli tktypes.mli $(CAMLTKDIR)/tkgen.mli
	cat tkbuiltin.mli tktypes.mli $(CAMLTKDIR)/tkgen.mli > tk.mli

TKMODULE=tk.mli

INTERFACES=\
   '%Tk' $(TKMODULE) \
   '%Widgets' $(GENERATED) \
   '%Support' $(SUPPORT)


mlifiles: tk.mli
	perl format-mli-files $(INTERFACES)

html: mlifiles
	latex2html -split 2  -dont_include reference camltk.tex

cleanhtml:
	cd camltk; rm -f node*.html camltk.html images.* labels.*

clean: 
	rm -f camltk.dvi camltk.log camltk.blg tk.mli inclmod.tex

scratch: clean
	rm -f camltk.blg camltk.toc $(GENFILES) *.aux

