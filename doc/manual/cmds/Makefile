FILES=install.tex comp.tex top.tex runtime.tex librar.tex lexyacc.tex \
  debugger.tex intf-c.tex

TRANSF=../../tools/transf
TEXQUOTE=../../tools/texquote2

all: $(FILES)

clean:
	rm -f $(FILES)
	rm -f *~ #*#

.SUFFIXES:
.SUFFIXES: .tex .etex

.etex.tex:
	$(TEXQUOTE) < $*.etex > $*.tex

lexyacc.tex: lexyacc.etex $(TRANSF)
	$(TRANSF) < lexyacc.etex | $(TEXQUOTE) > lexyacc.tex

