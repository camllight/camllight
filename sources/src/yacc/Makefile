# Makefile for the parser generator.

OUTPUT=camlyacc.exe

CFLAGS=-O -DNDEBUG

OBJS= closure.o error.o lalr.o lr0.o main.o mkpar.o output.o reader.o \
  skeleton.o symtab.o verbose.o warshall.o

all: camlyacc.exe

camlyacc.out: $(OBJS)
	$(CC) $(CFLAGS) -o camlyacc.out @&&!
$(OBJS)
!

install:
	copy camlyacc.exe $(BINDIR)\camlyacc.exe

!include "../tools/make-c.inc"

### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
