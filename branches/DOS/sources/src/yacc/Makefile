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
closure.o: closure.c defs.h ../../config/m.h ../../config/s.h
error.o: error.c defs.h ../../config/m.h ../../config/s.h
lalr.o: lalr.c defs.h ../../config/m.h ../../config/s.h
lr0.o: lr0.c defs.h ../../config/m.h ../../config/s.h
main.o: main.c defs.h ../../config/m.h ../../config/s.h
mkpar.o: mkpar.c defs.h ../../config/m.h ../../config/s.h
output.o: output.c defs.h ../../config/m.h ../../config/s.h
parsing.o: parsing.c
reader.o: reader.c defs.h ../../config/m.h ../../config/s.h
skeleton.o: skeleton.c defs.h ../../config/m.h ../../config/s.h
symtab.o: symtab.c defs.h ../../config/m.h ../../config/s.h
verbose.o: verbose.c defs.h ../../config/m.h ../../config/s.h
warshall.o: warshall.c defs.h ../../config/m.h ../../config/s.h
