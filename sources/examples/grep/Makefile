CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
CAMLDEP=perl /caml386/src/tools/camldep

all: grep.exe

grep.exe: expr.zo auto.zo ensent.zo determ.zo grep.zo
	$(CAMLC) $(LINKFLAGS) -o grep.exe expr.zo auto.zo ensent.zo determ.zo grep.zo

clean:
	del *.exe
	del *.zi
	del *.zo
	del *.zix

.mli.zi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.zo:
	$(CAMLC) $(COMPFLAGS) -c $<

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
