CC=tcc -mt
CFLAGS=
RUNTIME32=..\..\go32\go32.exe

CAMLC= camlc.obj driver.obj
CAML= caml.obj driver.obj

all: header camlc.exe caml.exe

header: camlexec.c
	$(CC) $(CFLAGS) -eheader. camlexec.c

install:
	copy header $(LIBDIR)
	copy camlc.exe $(BINDIR)
	copy caml.exe $(BINDIR)
	copy $(RUNTIME32) $(LIBDIR)

clean:
	del header
	del *.exe
	del *.obj

camlc.exe: $(CAMLC)
	$(CC) $(CFLAGS) -ecamlc.exe $(CAMLC)

caml.exe: $(CAML)
	$(CC) $(CFLAGS) -ecaml.exe $(CAML)

.c.obj:
	$(CC) $(CFLAGS) -c $<

camlc.obj: driver.h

