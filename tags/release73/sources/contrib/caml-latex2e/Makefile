# The directory where gglo.ist for makeindex is located
MAKEINDEX=/usr/lib/texmf/makeindex

# The directory where the public executable will be installed
BINDIR=/usr/local/bin

# The LaTeX input directory 
TEXINPUTDIR=/usr/lib/texmf/tex/latex/etc

### End of configuration section

caml: caml.sty

example: example.dvi

docu: caml.dvi

.SUFFIXES: .tex .dvi .dtx .sty .mlt

.dtx.dvi:
	latex $<
	makeindex -s $(MAKEINDEX)/gglo.ist -o $*.gls $*.glo
	latex $<

.tex.dvi:
	latex $<

.mlt.tex:
	caml-tex -o $*.tex $<

.dtx.sty:
	tex $*.ins

clean:
	rm -f *~ *.dvi *.log *.aux *.glo *.gls *.sty *.ilg *.idx *.tex

install:
	cp caml-tex $(BINDIR)/caml-tex
	cp caml.sty $(TEXINPUTDIR)/caml.sty
