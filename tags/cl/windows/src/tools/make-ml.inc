# Common make definitions for Caml programs

clean:
	rm -f $(OUTPUT) $(GENSOURCES)
	rm -f *.zo *.zi *.zix

version.ml: version.mlp ..\version.h
	$(CPP) version.mlp > version.ml

.SUFFIXES: .ml .mli .zo .zi

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

depend: $(GENSOURCES)
	mv makefile makefile.bak
	$(PERL) ..\tools\cutdeps makefile.bak > makefile
	$(PERL) ..\tools\camldep $(INCLUDES) >> makefile
	rm -f makefile.bak
