CC=gcc
CFLAGS=-O -DUSE_GCC
SMALLCC=wcl /ms

CAMLC=camlc.o driver.o ../runtime/aexpand.o
CAML=caml.o driver.o ../runtime/aexpand.o
CAMLMKTOP=camlmkto.o driver.o ../runtime/aexpand.o

all: header camlc.exe caml.exe camlmkto.exe

header: camlexec.c
	$(SMALLCC) /Fe=header. camlexec.c

install:
	copy header $(LIBDIR)
	copy camlc.exe $(BINDIR)
	copy caml.exe $(BINDIR)
	copy camlmkto.exe $(BINDIR)

clean:
	del header
	del *.exe
	del *.o

camlc.exe: $(CAMLC)
	$(CC) $(CFLAGS) -o camlc.exe $(CAMLC)

caml.exe: $(CAML)
	$(CC) $(CFLAGS) -o caml.exe $(CAML)

camlmkto.exe: $(CAMLMKTOP)
	$(CC) $(CFLAGS) -o camlmkto.exe $(CAMLMKTOP)

camlc.o caml.o camlmkto.o: driver.h

.c.o:
	$(CC) $(CFLAGS) -c $<
