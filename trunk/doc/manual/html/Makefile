FILES=foreword.tex biblio.tex 
VPATH=..

all: $(FILES) FORCE
	@$(MAKE) -f Makefile.refman
	@$(MAKE) -f Makefile.library
	@$(MAKE) -f Makefile.cmds
	../../tools/texexpand manual.tex \
        | ../../tools/htmlgen \
        | ../../tools/htmltbl > manual.html

FORCE:

clean:
	rm -f $(FILES)
	@$(MAKE) -f Makefile.refman clean
	@$(MAKE) -f Makefile.library clean
	@$(MAKE) -f Makefile.cmds clean

.SUFFIXES:
.SUFFIXES: .tex .etex

.etex.tex:
	cp $< $*.tex
