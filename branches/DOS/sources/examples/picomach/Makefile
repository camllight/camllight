CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
CAMLDEP=perl /caml386/src/tools/camldep

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

code.zo: code.zi 
simul.zo: simul.zi code.zi 
exec.zo: simul.zi code.zi 
lecture.zo: lecture.zi code.zi lexuniv.zi stockage.zi 
lexuniv.zo: lexuniv.zi 
loadall.zo: exec.zo asm.zo 
stockage.zo: stockage.zi code.zi 
lecture.zi: code.zi 
simul.zi: code.zi 
stockage.zi: code.zi 
asm.zo: lecture.zi stockage.zi 
