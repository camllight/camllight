## Configuration
include ../Makefile.config
CAMLTKDIR=../lib
SUPPORTDIR=../libsupport

COMPFLAGS=-g -O fast -I $(CAMLTKDIR) -I $(SUPPORTDIR)

COMPILEROBJS = config.zo misc.zo globals.zo modules.zo location.zo types.zo \
               errors.zo const.zo builtins.zo
OBJS= lexer.zo util.zo hyper_printers.zo visual.zo source.zo main.zo

all: camlbrowser

# # Two process architecture
# camlbrowser: $(COMPILEROBJS) $(OBJS)
# 	camlc -g -custom -I $(CAMLTKDIR) -o camlbrowser unix.zo \
#                tklib2.zo $(COMPILEROBJS) $(OBJS) -lunix

camlbrowser: $(COMPILEROBJS) $(OBJS)
	camlc -g -custom -I $(CAMLTKDIR) -o camlbrowser unix.zo \
                tklib1.zo $(COMPILEROBJS) $(OBJS) $(TKLINKOPT) -lunix

lexer.mll: lexer.mlp
	$(CPP) -Dunix lexer.mlp > lexer.mll

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

clean : 
	rm -f *.z* location.ml config.ml lexer.ml lexer.mll camlbrowser

.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo .mlp

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.mlp.ml:
	@rm -f $@
	$(CPP) -Dunix $< > $@
	@chmod a-w $@

depend:
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
	 $(CAMLDEP) *.mli *.ml) > Makefile

location.ml: location.mlp
config.ml: config.mlp

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
builtins.zo: misc.zo modules.zo globals.zo const.zo 
config.zo: config.zi 
const.zo: misc.zo 
errors.zo: misc.zo location.zi globals.zo const.zo 
globals.zo: prim.zo const.zo 
hyper_printers.zo: modules.zo const.zo misc.zo globals.zo builtins.zo \
    types.zo 
location.zo: location.zi misc.zo config.zi 
main.zo: source.zo modules.zo util.zo config.zi misc.zo visual.zo \
    hyper_printers.zo 
misc.zo: config.zi 
modules.zo: misc.zo errors.zo globals.zo const.zo 
prim.zo: const.zo 
source.zo: util.zo misc.zo lexer.zo 
types.zo: misc.zo modules.zo builtins.zo globals.zo const.zo 
visual.zo: modules.zo util.zo const.zo hyper_printers.zo globals.zo 
