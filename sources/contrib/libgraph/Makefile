# Makefile for the portable graphics library

# These options are overriden when called from ../Makefile
CC=cc
OPTS=
X11_INCLUDES=/usr/include
X11_LIB=/usr/lib
CPP=/lib/cpp -P

BINDIR=/usr/local/bin
LIBDIR=/usr/local/lib/caml-light

# Compilation options
CFLAGS=$(OPTS) -I../../src/runtime -I$(X11_INCLUDES) -O

# Test to see whether ranlib exists on the machine
RANLIBTEST=test -f /usr/bin/ranlib -o -f /bin/ranlib

# How to invoke ranlib
RANLIB=ranlib

# The Caml Light compilers
CAMLC=camlc
CAMLMKTOP=camlmktop
COMPFLAGS=-W -I ../libunix

OBJS=open.o draw.o fill.o color.o text.o \
  image.o make_img.o dump_img.o point_col.o sound.o events.o

GENFILES=graphics.ml

all: libgraph.a graphics.zo graphics.zi camlgraph

libgraph.a: $(OBJS)
	rm -f libgraph.a
	ar rc libgraph.a $(OBJS)
	if $(RANLIBTEST); then $(RANLIB) libgraph.a; else true; fi

camlgraph: libgraph.a graphics.zo
	$(CAMLMKTOP) -custom -o camlgraph ../libunix/unix.zo graphics.zo \
        libgraph.a ../libunix/libunix.a -ccopt -L$(X11_LIB) -lX11

clean:
	rm -f libgraph.a camlgraph $(GENFILES) *.o *.z[io] *~ #*#

install:
	cp libgraph.a $(LIBDIR)/libgraph.a
	if $(RANLIBTEST); then cd $(LIBDIR); $(RANLIB) libgraph.a; else true; fi
	cp graphics.z[io] $(LIBDIR)
	cp camlgraph $(LIBDIR)

.SUFFIXES: .ml .mli .zo .zi

.mli.zi:
	$(CAMLC) $(COMPFLAGS) -c $<
.ml.zo:
	$(CAMLC) $(COMPFLAGS) -c $<

graphics.ml: graphics.mlp
	@rm -f graphics.ml
	$(CPP) -Dunix graphics.mlp > graphics.ml
	@chmod -w graphics.ml

depend: $(GENFILES)
	mv Makefile Makefile.BAK; \
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.BAK; \
         gcc -MM $(CFLAGS) *.c; \
         ../../src/tools/camldep *.mli *.ml) > Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
color.o : color.c libgraph.h 
draw.o : draw.c libgraph.h 
dump_img.o : dump_img.c libgraph.h image.h 
events.o : events.c libgraph.h 
fill.o : fill.c libgraph.h 
image.o : image.c libgraph.h image.h 
make_img.o : make_img.c libgraph.h image.h 
open.o : open.c libgraph.h 
point_col.o : point_col.c libgraph.h 
sound.o : sound.c libgraph.h 
text.o : text.c libgraph.h 
graphics.zo: graphics.zi 
