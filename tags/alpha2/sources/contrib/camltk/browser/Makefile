TKDIR=../lib
CAMLCOMP=camlc -c
CPP=/lib/cpp -P -Dunix
CAMLLEX=camllex

COMPFLAGS=-g -O fast -I $(TKDIR)
LINKFLAGS=-g

COMPILEROBJS = config.zo misc.zo globals.zo modules.zo location.zo types.zo \
               errors.zo const.zo builtins.zo
OBJS= util.zo hyper_printers.zo visual.zo main.zo

CAMLDEP=../../../src/tools/camldep


camlbrowser: $(COMPILEROBJS) $(OBJS)
	camlc -g -custom -I $(TKDIR) -o camlbrowser unix.zo \
                tklib.zo $(COMPILEROBJS) $(OBJS) -lunix

clean : 
	rm -f *.z* location.ml config.ml  camlbrowser

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
info.zo: modules.zo globals.zo const.zo 
location.zo: location.zi misc.zo config.zi 
main.zo: modules.zo util.zo config.zi misc.zo visual.zo 
misc.zo: config.zi 
modules.zo: misc.zo errors.zo globals.zo const.zo 
prim.zo: const.zo 
types.zo: misc.zo modules.zo builtins.zo globals.zo const.zo 
visual.zo: modules.zo util.zo const.zo hyper_printers.zo globals.zo 
