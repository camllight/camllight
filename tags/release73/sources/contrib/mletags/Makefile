# Configuration
CPP=/lib/cpp -P
CAMLC=camlc
CAMLCOMP=$(CAMLC) -c -g
CAMLLEX=camllex
CAMLYACC=camlyacc

COMPFLAGS=-W -O fast
LINKFLAGS=-g

OBJS=lexer.zo parser.zo main.zo
GENFILES=lexer.ml parser.ml parser.mli lexer.mll

all: mletags

mletags: $(OBJS)
	camlc -o mletags $(LINKFLAGS) $(OBJS)

lexer.mll: lexer.mlp
	$(CPP) -Dunix lexer.mlp > lexer.mll

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

parser.ml parser.mli: parser.mly
# 	$(CAMLYACC) parser.mly
	$(CAMLYACC) -s parser.mly

clean:
	rm -f *.z* mletags $(GENFILES)

scratch: clean
	rm -f lexer.ml lexer.mll parser.ml parser.mli

distrib: main.ml parser.mly lexer.mlp tags.mli Makefile mletags.man INSTALL
	tar cf mletags.tar main.ml parser.mly lexer.mlp tags.mli Makefile mletags.man INSTALL
	compress mletags.tar

install: mletags mletags.man
	cp mletags $(BINDIR)
	cp mletags.man $(MANDIR)/mletags.$(MANEXT)

TAGS:
	./mletags *.ml *.mli

.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo .mlp

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.mlp.ml:
	@rm -f $@
	$(CPP) -Dunix $< > $@
	@chmod a-w $@

depend: $(GENFILES)
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
         ../../src/tools/camldep *.mli *.ml) > Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
lexer.zi: parser.zi 
parser.zi: tags.zi 
lexer.zo: lexer.zi parser.zi 
main.zo: parser.zi lexer.zi tags.zi 
parser.zo: parser.zi lexer.zi tags.zi 
