CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
COMPFLAGS=-W
CAMLDEP=perl /caml/src/tools/camldep

all: spir.exe spir-bw.exe

spir.exe: spir.ml
	$(CAMLC) $(LINKFLAGS) -o spir.exe spir.ml

spir-bw.exe: spir-bw.ml
	$(CAMLC) $(LINKFLAGS) -o spir-bw.exe spir-bw.ml

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

