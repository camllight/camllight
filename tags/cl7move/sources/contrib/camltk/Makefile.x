# Edit this file to reflect your installation
# save it as Makefile.config

## This should be the same compiler as for the rest
## of the Caml Light system
CC=cc

## Where the tcl.h and tk.h includes can be found
TCLINCLDIR=/usr/local/lib/tcl/include
TKINCLDIR=/usr/local/lib/tk/include

## Where the libtcl.a and libtk.a libraries can be found
TKLIBDIR=/usr/local/lib/tk/lib
TCLLIBDIR=/usr/local/lib/tcl/lib

## Where the camltk library will be installed.
## Do *not* set this to the same library as caml-light.
INSTALLDIR=/usr/local/lib/caml-light/tk

# camldep can be found in distrib/src/tools
# do NOT use a relative path ../../...
CAMLDEP=camldep

# You should not have to change these
CAMLC=camlc
CAMLCOMP=$(CAMLC) -c
CPP=/lib/cpp -P -Dunix

COMPFLAGS=-O fast -g
LINKFLAGS=-g
CAMLYACC=camlyacc -v
CAMLLEX=camllex

TKLINKOPT=-ccopt -L$(TCLLIBDIR) -ccopt -L$(TKLIBDIR) \
	   $(SUPPORTDIR)/camltk.o -lcaml -ltk -ltcl -lX11
