# Makefile for the Caml Light lexer generator

OUTPUT=camllex

INCLUDES=

OBJS=syntax.zo scan_aux.zo scanner.zo gram_aux.zo grammar.zo \
  lexgen.zo output.zo main.zo

GENFILES=grammar.ml grammar.mli scanner.ml

all: camllex

camllex: $(OBJS)
	$(CAMLLINK) $(LINKFLAGS) -o camllex stdlib.zo @&&!
$(OBJS)
!

install:
	..\tools\installb camllex $(BINDIR)\camllex

!include "../tools/make-ml.inc"

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
gram_aux.zo: syntax.zo 
iset.zo: iset.zi 
lexgen.zo: syntax.zo 
main.zo: lexgen.zo output.zo grammar.zi scanner.zo syntax.zo scan_aux.zo 
output.zo: syntax.zo 
scanner.zo: syntax.zo scan_aux.zo grammar.zi 
grammar.zo: grammar.zi gram_aux.zo syntax.zo 
grammar.zi: syntax.zo 
