CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
COMPFLAGS=-W
CAMLDEP=perl /caml/src/tools/camldep

all: grep.exe

grep.exe: expr.zo auto.zo ensent.zo determ.zo grep.zo
	$(CAMLC) $(LINKFLAGS) -o grep.exe expr.zo auto.zo ensent.zo determ.zo grep.zo

fastgrep.exe: expr.zo auto.zo ensent.zo determ.zo fastgrep.zo fastrec.c
	$(CAMLC) $(LINKFLAGS) -custom -o fastgrep.exe expr.zo auto.zo ensent.zo determ.zo fastgrep.zo fastrec.c

clean:
	del *.exe
	del *.zi
	del *.zo
	del *.zix
	del *.o

.mli.zi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.zo:
	$(CAMLC) $(COMPFLAGS) -c $<

depend:
	$(CAMLDEP) >> makefile

auto.zo: auto.zi expr.zi 
auto.zi: expr.zi 
determ.zo: determ.zi auto.zi ensent.zi 
determ.zi: auto.zi 
ensent.zo: ensent.zi 
expr.zo: expr.zi 
grep.zo: determ.zi auto.zi expr.zi 
loadall.zo: grep.zo 
fastgrep.zo: determ.zi auto.zi fastrec.zi expr.zi 
fastrec.zi: determ.zi 
