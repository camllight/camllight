CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
COMPFLAGS=-W
CAMLDEP=perl /caml/src/tools/camldep

all: logo.exe

logo.exe: crayon.zo langage.zo alex.zo asynt.zo logo.zo
	$(CAMLC) $(LINKFLAGS) -o logo.exe crayon.zo langage.zo alex.zo asynt.zo logo.zo

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

alex.zo: alex.zi 
asynt.zo: asynt.zi langage.zi alex.zi 
asynt.zi: langage.zi alex.zi 
crayon.zo: crayon.zi 
langage.zo: langage.zi crayon.zi 
loadall.zo: logo.zo 
logo.zo: asynt.zi langage.zi alex.zi 
