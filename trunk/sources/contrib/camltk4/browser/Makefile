# Overriden by caller
LIBDIR=/usr/local/lib/caml-light
BINDIR=/usr/local/bin

CAMLC=camlc
CAMLCOMP=$(CAMLC) -c
CPP=/lib/cpp -P -Dunix

CAMLYACC=camlyacc -v
CAMLLEX=camllex

# We compile before the library has been installed
CAMLTKDIR=../lib
SUPPORTDIR=../libsupport
UNIXDIR=../../libunix

# These options are overriden when called from ../Makefile
X11_LIB=/usr/X11R6/lib
TCLLIBDIR=/usr/local/lib/tcl7.4/lib
TKLIBDIR=/usr/local/lib/tk4.0/lib
TKLIBS=-ltk -ltcl -lX11

TKLINKOPT=-ccopt -L$(TCLLIBDIR) -ccopt -L$(TKLIBDIR) -ccopt -L$(X11_LIB) \
	  -ccopt -L$(SUPPORTDIR) -lcamltk4 $(TKLIBS)

CAMLDEP=../../../src/tools/camldep

COMPFLAGS=-W -g -O cautious -I $(CAMLTKDIR) -I $(SUPPORTDIR) -I $(UNIXDIR) \
          -I ../../../src/compiler -I ../frx
LINKFLAGS=-g -custom -I ../frx

COMPILEROBJS = config.zo misc.zo interntl.zo globals.zo modules.zo \
               location.zo types.zo \
               error.zo const.zo builtins.zo
OBJS= lexer.zo modlist.zo tags.zo source.zo \
      hyper_printers.zo visual.zo main.zo


all: camlbrowser

camlbrowser: $(OBJS) ../lib/tklib.zo
	$(CAMLC) $(LINKFLAGS) -I $(CAMLTKDIR) -I ../../../src/compiler \
		-o camlbrowser $(UNIXDIR)/unix.zo \
                tklib.zo libfrx.zo $(COMPILEROBJS) $(OBJS) $(TKLINKOPT) \
                $(UNIXDIR)/libunix.a

lexer.mll: lexer.mlp
	$(CPP) -Dunix lexer.mlp > lexer.mll

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll


main.ml: main.mlp
	@rm -f main.ml
	sed -e 's|LIBDIR|$(LIBDIR)|' main.mlp > main.ml
	@chmod -w main.ml

clean : 
	rm -f *.z* lexer.ml lexer.mll main.ml camlbrowser


install: camlbrowser
	cp camlbrowser $(BINDIR)
	cp camlbrowser.man $(MANDIR)/camlbrowser.$(MANEXT)
	-MLETAGS=`pwd`/../../mletags/mletags; \
	cd $(LIBDIR); \
	$${MLETAGS} *.ml *.mli

.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<


depend: main.ml lexer.ml
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
