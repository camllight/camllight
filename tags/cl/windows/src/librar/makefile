# Makefile for the Caml Light librarian.

OUTPUT=camllibr

INCLUDES=-I ../compiler -I ../linker

EXTERNOBJS=config.zo misc.zo interntl.zo readword.zo 

OBJS=librar.zo version.zo main.zo

GENSOURCES=version.ml

CAMLCOMP=..\camlrun ..\camlcomp
CAMLRUN=..\camlrun
CAMLLINK=..\camlrun ..\camllink
CAMLLIBR=..\camlrun ..\camllibr
CAMLLEX=..\camlrun ..\camllex
CAMLYACC=..\camlyacc -s
CPP=cl -EP -Dmsdos -Uunix
PERL=C:\bin\perl.exe
COMPFLAGS=-stdlib ../lib -W -O fast $(INCLUDES)
LINKFLAGS=-stdlib ../lib -g $(INCLUDES)

all: camllibr

camllibr: $(OBJS)
	$(CAMLLINK) $(LINKFLAGS) -o camllibr @<<
stdlib.zo $(EXTERNOBJS) $(OBJS)
<<

install:
	..\tools\installb camllibr $(BINDIR)\camllibr

!include "../tools/make-ml.inc"

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
main.zo: librar.zo version.zo 
