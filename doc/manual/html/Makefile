FILES=foreword.tex biblio.tex 
VPATH=..

all: $(FILES) FORCE
	@$(MAKE) -f Makefile.refman
	@$(MAKE) -f Makefile.library
	@$(MAKE) -f Makefile.cmds
	../../tools/texexpand manual.tex \
        | ../../tools/htmlgen \
        | ../../tools/htmltbl > manual.html
	cd man-caml; \
        ../../../tools/htmlcut -3 -11 -12 ../manual.html > index.html
	cd man-caml; ../../../tools/htmlthread node*.html

FORCE:

clean:
	rm -f $(FILES)
	@$(MAKE) -f Makefile.refman clean
	@$(MAKE) -f Makefile.library clean
	@$(MAKE) -f Makefile.cmds clean
	cd man-caml; rm -f index.html node*.html

.SUFFIXES:
.SUFFIXES: .tex .etex

.etex.tex:
	cp $< $*.tex
