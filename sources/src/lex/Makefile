# Makefile for the Caml Light lexer generator

CAMLCOMP=../camlrun ../camlcomp
CAMLLINK=../camlrun ../camllink
CAMLLEX=../camlrun ../camllex
CAMLYACC=../camlyacc -s

COMPFLAGS=-W -O fast -stdlib ../lib
LINKFLAGS=-g -stdlib ../lib

OBJS=syntax.zo scan_aux.zo scanner.zo gram_aux.zo grammar.zo \
  lexgen.zo output.zo main.zo

all: camllex

camllex: $(OBJS)
	$(CAMLLINK) $(LINKFLAGS) -o camllex stdlib.zo $(OBJS)

clean:
	rm -f *.zo *.zi camllex
	rm -f grammar.ml grammar.mli scanner.ml

install:
	cat $(LIBDIR)/header camllex > $(BINDIR)/camllex
	chmod a+x $(BINDIR)/camllex

grammar.ml grammar.mli: grammar.mly
	$(CAMLYACC) grammar.mly

grammar.zi: grammar.mli
	$(CAMLCOMP) $(COMPFLAGS) grammar.mli

scanner.ml: scanner.mll
	$(CAMLLEX) scanner.mll

.SUFFIXES:
.SUFFIXES: .ml .zo

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

depend: scanner.ml grammar.ml grammar.mli
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
         ../tools/camldep *.mli *.ml) > Makefile
	rm Makefile.bak

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
grammar.zi: syntax.zo 
gram_aux.zo: syntax.zo 
grammar.zo: grammar.zi gram_aux.zo syntax.zo 
lexgen.zo: syntax.zo 
main.zo: lexgen.zo output.zo grammar.zi scanner.zo syntax.zo scan_aux.zo 
output.zo: syntax.zo 
scanner.zo: syntax.zo scan_aux.zo grammar.zi 
