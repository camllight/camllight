CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
CAMLDEP=perl /caml386/src/tools/camldep

all: ipascal.exe cpascal.exe

ipascal.exe: lexuniv.zo syntaxe.zo valeur.zo envir.zo interp.zo typage.zo ipascal.zo
	$(CAMLC) $(LINKFLAGS) -o ipascal.exe lexuniv.zo syntaxe.zo valeur.zo envir.zo interp.zo typage.zo ipascal.zo

cpascal.exe: lexuniv.zo syntaxe.zo envir.zo typage.zo compil.zo cpascal.zo
	$(CAMLC) $(LINKFLAGS) -o cpascal.exe lexuniv.zo syntaxe.zo envir.zo typage.zo compil.zo cpascal.zo

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

compil.zi: syntaxe.zi 
envir.zo: envir.zi interp.zi syntaxe.zi 
envir.zi: syntaxe.zi 
interp.zi: syntaxe.zi 
lexuniv.zo: lexuniv.zi 
typage.zi: syntaxe.zi 
cpascal.zo: compil.zi typage.zi syntaxe.zi 
ipascal.zo: interp.zi typage.zi syntaxe.zi 
loadall.zo: ipascal.zo cpascal.zo 
typage.zo: typage.zi envir.zi syntaxe.zi 
valeur.zo: valeur.zi interp.zi 
compil.zo: compil.zi envir.zi typage.zi syntaxe.zi 
interp.zo: interp.zi valeur.zi envir.zi syntaxe.zi 
syntaxe.zo: syntaxe.zi lexuniv.zi 
