CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
COMPFLAGS=-W
CAMLDEP=perl /caml/src/tools/camldep

all: hanoi.exe

hanoi.exe: hanoi.zo
	$(CAMLC) $(LINKFLAGS) -o hanoi.exe hanoi.zo

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

loadall.zo: hanoi.zo 
