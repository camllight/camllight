# Makefile for the Caml Light librarian.

OUTPUT=camllibr

INCLUDES=-I ../compiler -I ../linker

EXTERNOBJS=config.zo misc.zo readword.zo 

OBJS=librar.zo version.zo main.zo

GENSOURCES=version.ml

all: camllibr

camllibr: $(OBJS)
	$(CAMLLINK) $(LINKFLAGS) -o camllibr stdlib.zo @&&!
$(EXTERNOBJS) $(OBJS)
!

install:
	..\tools\installb camllibr $(BINDIR)\camllibr

!include "../tools/make-ml.inc"

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
main.zo: librar.zo version.zo 
