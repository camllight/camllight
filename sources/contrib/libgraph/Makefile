# Makefile for the portable graphics library

### Configuration section

# The path to the Caml Light standard library
LIBDIR=/usr/local/lib/caml-light

# The path to the X11 include directory
# (usually /usr/include/X11; for Sparcstations, /usr/openwin/include/X11)
X11_INCLUDES=/usr/include/X11

# Which C compiler to use
CC=gcc
CFLAGS=-g -I../../src/runtime -I$(X11_INCLUDES)

# Test to see whether ranlib exists on the machine
RANLIBTEST=test -f /usr/bin/ranlib -o -f /bin/ranlib

# How to invoke ranlib
RANLIB=ranlib

### End of configuration section

CAMLC=camlc

OBJS=open.o draw.o fill.o color.o text.o \
  image.o make_img.o dump_img.o point_col.o sound.o events.o

all: libgraph.a graphics.zo graphics.zi

camlgraph: all
	camlmktop -custom -o camlgraph unix.zo graphics.zo libgraph.a -lunix -lX11

libgraph.a: $(OBJS)
	rm -f libgraph.a
	ar rc libgraph.a $(OBJS)
	ranlib libgraph.a

clean:
	rm -f libgraph.a camlgraph *.o *.z[io] *~ #*#

install:
	cp libgraph.a $(LIBDIR)/libgraph.a
	if $(RANLIBTEST); then cd $(LIBDIR); $(RANLIB) libgraph.a; else true; fi
	cp graphics.z[io] $(LIBDIR)

.SUFFIXES: .ml .mli .zo .zi

.mli.zi:
	$(CAMLC) -c $<
.ml.zo:
	$(CAMLC) -c $<

depend:
	mv Makefile Makefile.BAK; \
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.BAK; \
         gcc -MM $(CFLAGS) *.c; \
         ../../src/tools/camldep *.mli *.ml) > Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
color.o : color.c libgraph.h 
draw.o : draw.c libgraph.h 
dump_img.o : dump_img.c libgraph.h image.h 
fill.o : fill.c libgraph.h 
image.o : image.c libgraph.h image.h 
make_img.o : make_img.c libgraph.h image.h 
open.o : open.c libgraph.h 
point_col.o : point_col.c libgraph.h 
sound.o : sound.c libgraph.h 
test.o : test.c 
text.o : text.c libgraph.h 
graphics.zo: graphics.zi 
