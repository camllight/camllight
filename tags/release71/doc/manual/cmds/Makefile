FILES=install.tex comp.tex top.tex toplevel.tex \
  runtime.tex librar.tex lexyacc.tex \
  profil.tex debugger.tex emacs.tex intf-c.tex

TRANSF=../../tools/transf
TEXQUOTE=../../tools/texquote2
FORMAT=../../tools/format-intf

all: $(FILES)

clean:
	rm -f $(FILES)
	rm -f *~ #*#

.SUFFIXES:
.SUFFIXES: .tex .etex

.etex.tex:
	$(TEXQUOTE) < $*.etex > $*.tex

TOPLEVELMLI=../../../cl/src/toplevel/toplevel.mli

toplevel.tex: $(TOPLEVELMLI)
	$(FORMAT) toplevel $(TOPLEVELMLI) | tail +2 > toplevel.tex

lexyacc.tex: lexyacc.etex $(TRANSF)
	$(TRANSF) < lexyacc.etex | $(TEXQUOTE) > lexyacc.tex

debugger.tex: debugger.etex $(TRANSF)
	$(TRANSF) < debugger.etex | $(TEXQUOTE) > debugger.tex

