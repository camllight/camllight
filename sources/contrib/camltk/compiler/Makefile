CAMLC=camlc
CAMLCOMP=$(CAMLC) -c
CPP=/lib/cpp -P -Dunix
COMPFLAGS=-W -O fast -g
LINKFLAGS=-g
CAMLYACC=camlyacc
CAMLLEX=camllex
CAMLDEP=../../../src/tools/camldep

OBJS=tsort.zo tables.zo lexer.zo parser.zo  compile.zo intf.zo maincompile.zo

tkcompiler : $(OBJS)
	$(CAMLC) $(LINKFLAGS) -o tkcompiler $(OBJS)

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

parser.ml parser.mli: parser.mly
	$(CAMLYACC) parser.mly

clean : 
	rm -f *.z*  parser.ml lexer.ml tkcompiler

scratch :
	rm -f *.z*  parser.ml lexer.ml tkcompiler

install: 
	cp tkcompiler $(BINDIR)

.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo .mlp

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

depend: parser.ml parser.mli lexer.ml
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
	 $(CAMLDEP) *.mli *.ml) > Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
compile.zo: tables.zo 
intf.zo: compile.zo tables.zo 
lexer.zo: parser.zi 
maincompile.zo: parser.zi lexer.zo compile.zo intf.zo tsort.zo tables.zo 
parser.zo: parser.zi tables.zo 
tables.zo: tsort.zo 
