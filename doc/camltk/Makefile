LATEX=latex
DIR=../../cl/contrib/camltk
CAMLTKDIR= $(DIR)/lib
TKSUPDIR=$(DIR)/libsupport

# THIS MAKEFILE IS BOGUS, DON'T RELY ON DEPENDANCIES

TEXFILES=camltk.tex user.tex impl.tex reference.tex bnf.tex
GENFILES=inclmod.tex bindtypes.mli tktypes.mli helloworld.ml \
	 addition.ml tkgen.mli textvariable.mli tk.mli

.SUFFIXES: .tex .bib .bbl


all: camltk.dvi html

camltk.dvi: $(TEXFILES) $(GENFILES) camltk.bbl
	$(LATEX) camltk.tex

camltk.bbl: tk.bib
	bibtex camltk

camltk.aux: camltk.tex
	$(LATEX) camltk.tex

###### Generated files
# Inclusion of all modules
inclmod.tex: $(CAMLTKDIR)/modules makemodinc
	CAMLTKDIR=$(CAMLTKDIR); export CAMLTKDIR; ./makemodinc > inclmod.tex

# Types from builtin_bind
bindtypes.mli: $(TKSUPDIR)/builtin_bind.ml
	rm -f bindtypes.mli
	(echo '\begin{verbatim}'; \
	 sed -n -e '/^type/,/^;;$$/p' $(TKSUPDIR)/builtin_bind.ml; \
	 echo '\end{verbatim}') > bindtypes.mli

# Types from tk
tktypes.mli: $(CAMLTKDIR)/tk.ml
	rm -f tktypes.mli
	(echo '\begin{verbatim}'; \
	 sed -n -e '/^type/,/^;;$$/p' $(CAMLTKDIR)/tk.ml; \
	 echo '\end{verbatim}') | expand > tktypes.mli

# Other files included in the doc
helloworld.ml:
	-ln -s $(DIR)/test/helloworld.ml ./helloworld.ml

addition.ml:
	-ln -s $(DIR)/test/addition.ml ./addition.ml

tkgen.mli:
	-ln -s $(CAMLTKDIR)/tkgen.mli ./tkgen.mli

textvariable.mli:
	-ln -s $(TKSUPDIR)/textvariable.mli ./textvariable.mli

clean: 
	rm -f camltk.dvi camltk.log camltk.blg $(GENFILES)

scratch: clean
	rm -f camltk.blg camltk.bbl camltk.toc  *.aux

## The WWW Documentation
## REQUIRES our custom psearch cgi script

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
	[ -d camltk ] || mkdir camltk
	[ -d camltk/library ] || mkdir camltk/library
	[ -f camltk/library-header.html ] || \
		cp library-header.html camltk/library-header.html
	perl format-mli-files $(INTERFACES)

html: mlifiles
	latex2html -split 2  -dont_include reference camltk.tex

cleanhtml:
	rm -rf camltk
