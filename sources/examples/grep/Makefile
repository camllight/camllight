CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
CAMLDEP=perl /caml386/src/tools/camldep

all: grep.exe

grep.exe: expr.zo auto.zo ensent.zo determ.zo grep.zo
	$(CAMLC) $(LINKFLAGS) -o grep.exe expr.zo auto.zo ensent.zo determ.zo grep.zo

fastgrep.exe: expr.zo auto.zo ensent.zo determ.zo fastgrep.zo fastrec.o
	$(CAMLC) $(LINKFLAGS) -custom -o fastgrep.exe expr.zo auto.zo ensent.zo determ.zo fastgrep.zo fastrec.o

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
.c.o:
	$(CAMLC) -c $<

depend:
	$(CAMLDEP) >> makefile

determ.zi: auto.zi 
determ.zo: determ.zi auto.zi ensent.zi 
auto.zo: auto.zi expr.zi 
loadall.zo: grep.zo 
ensent.zo: ensent.zi 
expr.zo: expr.zi 
grep.zo: determ.zi auto.zi expr.zi 
auto.zi: expr.zi 
fastrec.zi: determ.zi 
fastgrep.zo: determ.zi auto.zi fastrec.zi expr.zi 
