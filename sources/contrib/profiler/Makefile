CPP=/lib/cpp -P

CAMLC=camlc
CAMLCOMP=$(CAMLC) -c
CAMLLEX=camllex
CAMLYACC=camlyacc

COMPFLAGS=-W -g -I ../../src/compiler
LINKFLAGS=-g -I ../../src/compiler

# Pieces of the compiler required for parsing
COMPILEROBJS=config.zo misc.zo interntl.zo globals.zo location.zo syntax.zo \
  modules.zo builtins.zo types.zo pr_type.zo error.zo typing.zo par_aux.zo \
  primdecl.zo ty_decl.zo config.zo parser.zo lexer.zo ty_intf.zo

# Source rewriting
OBJS=camlpro.zo

# Standard library for the profiler

PRO_IMPL= bool.zo eq.zo exc.zo int.zo fstring.zo string.zo fchar.zo \
    char.zo float.zo list.zo fvect.zo vect.zo profiler.zo io.zo pair.zo \
    arg.zo filename.zo hashtbl.zo lexing.zo parsing.zo printexc.zo sort.zo \
    printf.zo stream.zo stack.zo queue.zo format.zo random.zo genlex.zo \
    baltree.zo set.zo map.zo

all: camlpro stdlibp.zo prodump.zo

camlpro: $(OBJS)
	camlc $(LINKFLAGS) -o camlpro $(COMPILEROBJS) $(OBJS)

camlpro.ml: camlpro.mlp
	@rm -f camlpro.ml
	sed -e 's|LIBDIR|$(LIBDIR)|' camlpro.mlp > camlpro.ml
	@chmod -w camlpro.ml

prodump.zo: prodump.ml
	camlc -c -O none prodump.ml

stdlibp.zo: profiler.ml profiler.mli io.ml
	camlc -c -O none profiler.mli
	camlc -c -O none profiler.ml
	/bin/rm -f io.mli
	-ln -s ../../src/lib/io.mli ./io.mli
	camlc -c -O none io.mli
	camlc -c -O none io.ml
	camllibr -stdlib ../../src/lib -o stdlibp.zo $(PRO_IMPL)

install: 
	cp camlpro $(LIBDIR)/camlinstr
	rm -f $(BINDIR)/camlpro
	ln -s $(LIBDIR)/camlinstr $(BINDIR)/camlpro
	cp stdlibp.zo prodump.zo profiler.zi $(LIBDIR)

clean:
	rm -f *.z[oi] camlinstr camlpro io.mli camlpro.ml

scratch:
	rm -f *.z[oi] camlinstr camlpro io.mli
	rm -f camlpro.tar.Z camlpro.dump
	
.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo .mlp

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.mlp.ml:
	$(CPP) -Dunix $< > $@

depend: camlpro.ml
	mv Makefile Makefile.BAK; \
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.BAK; \
         ../../src/tools/camldep *.mli *.ml) > Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
camlpro.zo: profiler.zi 
io.zo: io.zi profiler.zi 
prodump.zo: io.zi 
profiler.zo: profiler.zi 
