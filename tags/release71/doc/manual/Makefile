FILES=allfiles.tex biblio.tex foreword.tex
DVI2TXT=../tools/dvi2txt
TEXINPUTS=.:..:../refman:../library:../cmds:../../styles:/usr/local/lib/tex/inputs
TEXFONTS=../../styles:/usr/local/lib/tex/fonts
RELEASE=$$HOME/release/$${RELEASENAME}

manual: files
	cd texstuff; \
	TEXINPUTS=$(TEXINPUTS) latex manual.tex

index::
	cd texstuff; makeindex manual.idx
	cd texstuff; makeindex manual.kwd.idx

plaintext: files
	cd texstuff; \
	TEXINPUTS=$(TEXINPUTS) TEXFONTS=$(TEXFONTS) latex plaintext.tex
	$(DVI2TXT) texstuff/plaintext.dvi > texstuff/plaintext.txt
	$(DVI2TXT) -p texstuff/plaintext.dvi > texstuff/plaintext.ipr

index::
	cd texstuff; makeindex plaintext.idx
	cd texstuff; makeindex plaintext.kwd.idx

scrolltext: files
	cd texstuff; \
	TEXINPUTS=$(TEXINPUTS) TEXFONTS=$(TEXFONTS) latex scrolltext.tex
	$(DVI2TXT) -s -t texstuff/scrolltext.dvi

files: $(FILES)
	cd refman; $(MAKE) all
	cd library; $(MAKE) all
	cd cmds; $(MAKE) all

all:
	$(MAKE) manual plaintext
	$(MAKE) manual plaintext
	$(MAKE) index
	$(MAKE) manual plaintext
	cd html; $(MAKE)

clean:
	rm $(FILES)
	cd refman; $(MAKE) clean
	cd library; $(MAKE) clean
	cd cmds; $(MAKE) clean
	rm -f texstuff/*

release:
	gzip < texstuff/manual.dvi > $(RELEASE)refman.dvi.gz
	dvips -Pps -o '!gzip > $(RELEASE)refman.ps.gz' texstuff/manual.dvi
	cp texstuff/plaintext.txt $(RELEASE)refman.txt
	cp texstuff/plaintext.ipr $(RELEASE)refman.prn
	cd html; $(MAKE) release

.SUFFIXES:
.SUFFIXES: .tex .etex

.etex.tex:
	../tools/texquote2 < $*.etex > $*.tex
