CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
CAMLDEP=perl /caml7/src/tools/camldep

all: demonstr.exe

demonstr.exe: prop.zo lexuniv.zo asynt.zo demo.zo
	$(CAMLC) $(LINKFLAGS) -o demonstr.exe prop.zo lexuniv.zo asynt.zo demo.zo

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

asynt.zo: asynt.zi prop.zi lexuniv.zi 
asynt.zi: prop.zi 
demo.zo: asynt.zi prop.zi 
lexuniv.zo: lexuniv.zi 
loadall.zo: demo.zo 
prop.zo: prop.zi 
