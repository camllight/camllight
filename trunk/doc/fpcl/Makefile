CAMLLATEX=../tools/caml-tex -n 80
DVI2TXT=../tools/dvi2txt
RELEASE=$$HOME/release/$${RELEASENAME}

CHAPTERS = Gen.mltex Fund.mltex Basic.mltex Lists.mltex \
  Types.mltex Annot.mltex Exc.mltex IO.mltex Streams.mltex \
  Modules.tex ASL.mltex Ans.mltex

fpcl: $(CHAPTERS)
	TEXINPUTS=../styles:$$TEXINPUTS latex fpcl.tex

plaintext: $(CHAPTERS)
	TEXINPUTS=../styles:$$TEXINPUTS \
        TEXFONTS=../styles:/usr/local/lib/tex/fonts \
        latex plaintext.tex
	$(DVI2TXT) plaintext.dvi > plaintext.txt
	$(DVI2TXT) -p plaintext.dvi > plaintext.ipr

biblio:
	bibtex fpcl
	bibtex plaintext

all:
	$(MAKE) fpcl plaintext
	$(MAKE) biblio
	$(MAKE) fpcl plaintext
	$(MAKE) fpcl plaintext

clean:
	rm -f *.mltex *~ *.log *.aux *.dvi *.bbl *.blg *.idx *.ilg *.ind *.toc
	rm -f *.txt *.ps *.ipr

release:
	gzip < fpcl.dvi > $(RELEASE)tutorial.dvi.gz
	dvips -Pps -o '!gzip > $(RELEASE)tutorial.ps.gz' fpcl.dvi
	cp plaintext.txt $(RELEASE)tutorial.txt
	cp plaintext.ipr $(RELEASE)tutorial.prn

.SUFFIXES: .tex .mltex

.tex.mltex:
	$(CAMLLATEX) -o $*.mltex $<
