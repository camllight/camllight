CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
CAMLDEP=perl /caml386/src/tools/camldep

all: minicaml.exe

minicaml.exe: eval.zo lexuniv.zo syntaxe.zo types.zo synthese.zo caml.zo
	$(CAMLC) $(LINKFLAGS) -o minicaml.exe eval.zo lexuniv.zo syntaxe.zo types.zo synthese.zo caml.zo

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

interpre.zo: eval.zi syntaxe.zi 
lexuniv.zo: lexuniv.zi 
loadall.zo: caml.zo 
synthese.zo: synthese.zi syntaxe.zi types.zi 
types.zo: types.zi 
typeur.zo: eval.zi syntaxe.zi synthese.zi types.zi 
synthese.zi: syntaxe.zi types.zi 
caml.zo: eval.zi syntaxe.zi synthese.zi types.zi 
eval.zi: syntaxe.zi 
eval.zo: eval.zi syntaxe.zi 
syntaxe.zo: syntaxe.zi lexuniv.zi 
