CPP=/lib/cpp -P -Dunix

CAMLCOMP=camlc -c
CAMLLEX=camllex
CAMLYACC=camlyacc

COMPFLAGS=-g -I ../../src/compiler
LINKFLAGS=-g -I ../../src/compiler

# Pieces of the compiler required for parsing
COMPILEROBJS=misc.zo globals.zo modules.zo types.zo syntax.zo error.zo \
	builtins.zo location.zo typing.zo par_aux.zo primdecl.zo ty_decl.zo \
	config.zo parser.zo lexer.zo ty_intf.zo  
# Source rewriting
OBJS=camlpro.zo

# Standard library for the profiler

PRO_IMPL= eq.zo exc.zo int.zo float.zo fstring.zo string.zo fchar.zo \
    char.zo list.zo fvect.zo vect.zo profiler.zo io.zo list.zo pair.zo arg.zo \
    filename.zo hashtbl.zo lexing.zo parsing.zo printexc.zo sort.zo \
    printf.zo stream.zo stack.zo queue.zo random.zo genlex.zo \
    baltree.zo set.zo map.zo

all: camlpro stdlibp.zo prodump.zo
	
camlpro: $(OBJS)
	camlc $(LINKFLAGS) -o camlpro $(COMPILEROBJS) $(OBJS)

prodump.zo: prodump.ml
	camlc -c -O none prodump.ml

stdlibp.zo: profiler.ml profiler.mli io.ml
	camlc -c -O none profiler.mli
	camlc -c -O none profiler.ml
	/bin/rm -f io.mli
	-ln -s ../../src/lib/io.mli
	camlc -c -O none io.mli
	camlc -c -O none io.ml
	camllibr -stdlib ../../src/lib -o stdlibp.zo $(PRO_IMPL)

install: 
	cp camlpro $(LIBDIR)/camlinstr
	rm -f $(BINDIR)/camlpro
	ln -s $(LIBDIR)/camlinstr $(BINDIR)/camlpro
	cp stdlibp.zo prodump.zo profiler.zi $(LIBDIR)

clean:
	rm -f *.z[oi] camlinstr camlpro io.mli

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
	$(CPP) $< > $@
