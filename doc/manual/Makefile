FILES=allfiles.tex biblio.tex foreword.tex
DVI2TXT=../tools/dvi2txt
TEXINPUTS=.:..:../refman:../library:../cmds:../../styles:/usr/local/lib/tex/inputs
TEXFONTS=../../styles:/usr/local/lib/tex/fonts
RELEASE=../../rel6/cl6

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
	@cd refman; make all
	@cd library; make all
	@cd cmds; make all

all:
	make manual plaintext scrolltext
	make manual plaintext scrolltext
	make index
	make manual plaintext

clean:
	rm $(FILES)
	@cd refman; make clean
	@cd library; make clean
	@cd cmds; make clean
	rm -f texstuff/*

release:
	compress < texstuff/manual.dvi > $(RELEASE)refman.dvi.Z
	dvips -Plw -o '!compress > $(RELEASE)refman.ps.Z' texstuff/manual.dvi
	cp texstuff/plaintext.txt $(RELEASE)refman.txt
	cp texstuff/plaintext.ipr $(RELEASE)refman.prn

.SUFFIXES:
.SUFFIXES: .tex .etex

.etex.tex:
	../tools/texquote2 < $*.etex > $*.tex
