CAMLTKDIR=../lib
SUPPORTDIR=../libsupport

CAMLC=camlc
CAMLCOMP=$(CAMLC) -c
CPP=/lib/cpp -P -Dunix
COMPFLAGS=-g -O fast -I $(CAMLTKDIR) -I $(SUPPORTDIR) 
LINKFLAGS=-g
CAMLYACC=camlyacc -v
CAMLLEX=camllex
CAMLDEP=../../../src/tools/camldep

LINKFLAGS=-g -custom
CAMLYACC=camlyacc -v
CAMLLEX=camllex

TKLINKOPT=-ccopt -L$(TCLLIBDIR) -ccopt -L$(TKLIBDIR) -ccopt -L$(X11_LIB) \
	   $(SUPPORTDIR)/camltk.o -lcaml -ltk -ltcl -lX11

OBJS = lexer.zo emacsbindings.zo sessions.zo slavetop.zo top.zo  main.zo

# Two process architecture
top: $(OBJS)
	$(CAMLC) $(LINKFLAGS) -I $(CAMLTKDIR) -o top unix.zo \
                tklib2.zo $(OBJS) -lunix

lexer.mll: lexer.mlp
	$(CPP) -Dunix lexer.mlp > lexer.mll

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

clean : 
	rm -f *.z? top lexer.ml lexer.mll

.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo .mlp

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
main.zo: slavetop.zo top.zo 
test.zo: slavetop.zo top.zo emacsbindings.zo 
top.zo: slavetop.zo lexer.zo emacsbindings.zo sessions.zo 
