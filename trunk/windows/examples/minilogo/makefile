CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
CAMLDEP=perl /caml7/src/tools/camldep

all: logo.exe

logo.exe: crayon.zo langage.zo alex.zo asynt.zo logo.zo
	$(CAMLC) $(LINKFLAGS) -o logo.exe crayon.zo langage.zo alex.zo asynt.zo logo.zo

clean:
	del *.exe
	del *.zi
	del *.zo
	del *.zix

.SUFFIXES: .ml .mli .zo .zi

.mli.zi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.zo:
	$(CAMLC) $(COMPFLAGS) -c $<

depend:
	$(CAMLDEP) >> makefile

