## Configuration
include ../Makefile.config

OBJS=tsort.zo tables.zo lexer.zo parser.zo  compile.zo intf.zo maincompile.zo

tkcompiler : $(OBJS)
	camlc -g -o tkcompiler $(OBJS)
#	camldbgc -g -o tkcompiler $(OBJS)

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

parser.ml parser.mli: parser.mly
	$(CAMLYACC) parser.mly

clean : 
	rm -f *.z*  parser.ml lexer.ml tkcompiler

scratch :
	rm -f *.z*  parser.ml lexer.ml tkcompiler

install: 
	cp -p tkcompiler $(INSTALLDIR)

.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo .mlp

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

depend:
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
	 $(CAMLDEP) *.mli *.ml) > Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
compile.zo: tables.zo 
intf.zo: compile.zo tables.zo 
lexer.zo: parser.zi 
maincompile.zo: parser.zi lexer.zo compile.zo intf.zo tsort.zo tables.zo 
tables.zo: tsort.zo 
