# Overriden by caller
LIBDIR=/usr/local/lib/caml-light
BINDIR=/usr/local/bin

CAMLC=camlc
CAMLCOMP=$(CAMLC) -c
CPP=/lib/cpp -P -Dunix

LINKFLAGS=-g -custom
CAMLYACC=camlyacc -v
CAMLLEX=camllex

# We compile before the library has been installed
CAMLTKDIR=../lib
SUPPORTDIR=../libsupport
UNIXDIR=../../libunix

TKLINKOPT=-ccopt -L$(TCLLIBDIR) -ccopt -L$(TKLIBDIR) -ccopt -L$(X11_LIB) \
	  -ccopt -L$(SUPPORTDIR) \
	   -lcaml -lcamltk -ltk -ltcl -lX11

CAMLDEP=../../../src/tools/camldep

COMPFLAGS=-g -O fast -I $(CAMLTKDIR) -I $(SUPPORTDIR) -I $(UNIXDIR) \
          -I ../../../src/compiler

COMPILEROBJS = config.zo misc.zo interntl.zo globals.zo modules.zo \
               location.zo types.zo \
               error.zo const.zo builtins.zo
OBJS= lexer.zo util.zo hyper_printers.zo visual.zo source.zo main.zo

all: camlbrowser

# # Two process architecture
# camlbrowser: $(COMPILEROBJS) $(OBJS)
# 	$(CAMLC) $(LINKFLAGS) -I $(CAMLTKDIR) -o camlbrowser $(UNIXDIR)/unix.zo \
#                tklib2.zo $(COMPILEROBJS) $(OBJS) $(UNIXDIR)/libunix.a

camlbrowser: $(OBJS)
	$(CAMLC) $(LINKFLAGS) -I $(CAMLTKDIR) -I ../../../src/compiler \
		-o camlbrowser $(UNIXDIR)/unix.zo \
                tklib.zo $(COMPILEROBJS) $(OBJS) $(TKLINKOPT) \
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
	cp -p camlbrowser $(BINDIR)

.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<


depend:
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
	 $(CAMLDEP) *.mli *.ml) > Makefile


### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
main.zo: source.zo util.zo visual.zo hyper_printers.zo 
source.zo: util.zo visual.zo 
visual.zo: util.zo hyper_printers.zo 
