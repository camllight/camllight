LATEX=latex
FORMAT=../tools/format-intf

DIR=../../cl/contrib/camltk
CAMLTKDIR= $(DIR)/lib
TKSUPDIR=$(DIR)/libsupport


# THIS MAKEFILE IS BOGUS, DON'T RELY ON DEPENDANCIES

# Source files
TEXFILES=camltk.tex user.tex impl.tex reference.tex bnf.tex

# Generated files
GENFILES=inclmod.tex \
	bindtypes.tex tkbuiltin.tex tktypes.tex helloworld.tex \
	addition.tex tkgen.tex textvariable.tex

all: camltk.dvi

camltk.dvi: $(TEXFILES) $(GENFILES) $(MODULES) camltk.bbl
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
bindtypes.tex: $(TKSUPDIR)/builtin_bind.ml
	rm -f bindtypes.tex
	(echo '\begin{verbatim}'; \
	 sed -n -e '/^type/,/^;;$$/p' $(TKSUPDIR)/builtin_bind.ml; \
	 echo '\end{verbatim}') | expand > bindtypes.tex

# Types from tk
tktypes.tex: $(CAMLTKDIR)/tk.ml
	rm -f tktypes.tex
	(echo '\begin{verbatim}'; \
	sed -n -e '/^type/,/^;;$$/p' $(CAMLTKDIR)/tk.ml; \
	echo '\end{verbatim}') | expand > tktypes.tex

tkbuiltin.tex:
	$(FORMAT) "tk (builtin functions)" < tkbuiltin.mli > tkbuiltin.tex

tkgen.tex:
	(echo '(* user defined functions *)'; echo "" ; \
	cat $(CAMLTKDIR)/tkgen.mli) | \
	$(FORMAT) "tk (other functions)" > tkgen.tex

textvariable.tex:
	$(FORMAT) textvariable < $(TKSUPDIR)/textvariable.mli > textvariable.tex


# Other files included in the doc
helloworld.tex:
	rm -rf helloworld.tex
	(echo '\begin{verbatim}'; \
	 cat $(DIR)/test/helloworld.ml; \
	echo '\end{verbatim}') | expand > helloworld.tex

addition.tex:
	rm -rf addition.tex
	(echo '\begin{verbatim}'; \
	cat $(DIR)/test/addition.ml; \
	echo '\end{verbatim}') | expand > addition.tex

clean: 
	rm -f camltk.dvi camltk.log camltk.blg $(GENFILES)

scratch: clean
	rm -f camltk.blg camltk.bbl camltk.toc  *.aux


.SUFFIXES:
.SUFFIXES: .tex .mli .bib .bbl

.mli.tex:
	$(FORMAT) $* $< > $*.tex

.ml.tex:
	$(FORMAT) $* $< > $*.tex
