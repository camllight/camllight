CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
COMPFLAGS=-W
CAMLDEP=perl /caml/src/tools/camldep

all: asm.exe exec.exe

asm.exe: code.zo stockage.zo lexuniv.zo lecture.zo asm.zo
	$(CAMLC) $(LINKFLAGS) -o asm.exe code.zo stockage.zo lexuniv.zo lecture.zo asm.zo

exec.exe: code.zo simul.zo exec.zo
	$(CAMLC) $(LINKFLAGS) -o exec.exe code.zo simul.zo exec.zo

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

asm.zo: lecture.zi stockage.zi 
code.zo: code.zi 
exec.zo: simul.zi code.zi 
lecture.zo: lecture.zi code.zi lexuniv.zi stockage.zi 
lecture.zi: code.zi 
lexuniv.zo: lexuniv.zi 
loadall.zo: exec.zo asm.zo 
simul.zo: simul.zi code.zi 
simul.zi: code.zi 
stockage.zo: stockage.zi code.zi 
stockage.zi: code.zi 
