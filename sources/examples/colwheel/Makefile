CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
COMPFLAGS=-W
CAMLDEP=perl /caml/src/tools/camldep

all: colwheel.exe

colwheel.exe: colwheel.ml
	$(CAMLC) $(LINKFLAGS) -o colwheel.exe colwheel.ml

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

