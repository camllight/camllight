# Makefile for the Caml Light toplevel.

CAMLCOMP=../camlrun ../camlcomp
CAMLLINK=../camlrun ../camllink
CAMLLIBR=../camlrun ../camllibr
CAMLRUN=../camlrun
INCLUDES=-stdlib ../lib -I ../compiler -I ../linker
COMPFLAGS=-W -O fast $(INCLUDES)
LINKFLAGS=-g $(INCLUDES)
CPP=/lib/cpp -P -Dunix
CPPFLAGS=

EXTERNOBJS=config.zo misc.zo interntl.zo \
    const.zo prim.zo instruct.zo lambda.zo \
    globals.zo location.zo syntax.zo modules.zo builtins.zo \
    types.zo pr_type.zo error.zo typing.zo ty_decl.zo pr_decl.zo \
    ty_intf.zo tr_env.zo event.zo clauses.zo matching.zo trstream.zo front.zo \
    back.zo opcodes.zo prim_opc.zo buffcode.zo labels.zo \
    reloc.zo emitcode.zo emit_phr.zo \
    primdecl.zo lexer.zo par_aux.zo parser.zo compiler.zo \
    predef.zo prim_c.zo symtable.zo patch.zo tr_const.zo

OBJS=fmt_type.zo pr_value.zo load_phr.zo do_phr.zo toplevel.zo \
     version.zo topinit.zo

PERVASIVES=baltree bool char eq exc fchar filename float format fstring fvect \
    gc genlex hashtbl int io iparsing lexing list map obj pair parsing \
    printexc printf queue random ref set sort stack stream string \
    toplevel vect

SPECIALS=sys

GENSOURCES=version.ml pr_value.ml

all: camltop

camltop: toplib.zo topmain.zo provide expunge
	$(CAMLRUN) ./provide -stdlib ../lib $(PERVASIVES) > required
	$(CAMLLINK) $(LINKFLAGS) -o camltop1 -require required \
                stdlib.zo toplib.zo topmain.zo
	$(CAMLRUN) ./expunge camltop1 camltop $(PERVASIVES) $(SPECIALS)
	rm -f camltop1 required
	cp toplevel.zi ../lib

toplib.zo: $(OBJS)
	$(CAMLLIBR) -o toplib.zo $(INCLUDES) $(EXTERNOBJS) $(OBJS)

expunge: expunge.zo
	$(CAMLLINK) $(LINKFLAGS) -o expunge stdlib.zo readword.zo expunge.zo

provide: provide.zo
	$(CAMLLINK) $(LINKFLAGS) -o provide stdlib.zo \
                config.zo misc.zo interntl.zo modules.zo provide.zo

clean:
	rm -f *.zi *.zo camltop expunge provide
	rm -f $(GENSOURCES)

install:
	cp camltop $(LIBDIR)/camltop
	cp toplevel.mli toplevel.zi $(LIBDIR)
	cp provide $(LIBDIR)/provide
	cp expunge $(LIBDIR)/expunge
	cp toplib.zo $(LIBDIR)/toplib.zo
	cp topmain.zo $(LIBDIR)/topmain.zo

.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo .mlp

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.mlp.ml:
	@rm -f $@
	$(CPP) $(CPPFLAGS) $< > $@
	@chmod a-w $@

depend: $(GENSOURCES)
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
         ../tools/camldep -I ../compiler -I ../linker meta.mli *.ml) > Makefile
	rm Makefile.bak

version.ml: version.mlp ../version.h
pr_value.ml: pr_value.mlp

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
do_phr.zo: pr_value.zo ../compiler/typing.zo meta.zi ../compiler/modules.zo \
    ../compiler/interntl.zo ../compiler/compiler.zo ../compiler/const.zo \
    ../compiler/misc.zo load_phr.zo ../compiler/back.zo \
    ../compiler/front.zo fmt_type.zo ../linker/symtable.zo \
    ../compiler/syntax.zo ../compiler/ty_decl.zo ../compiler/types.zo 
expunge.zo: ../linker/symtable.zo ../compiler/const.zo 
fmt_type.zo: ../compiler/modules.zo ../compiler/globals.zo \
    ../compiler/types.zo ../compiler/const.zo 
load_phr.zo: pr_value.zo ../compiler/reloc.zo meta.zi ../linker/tr_const.zo \
    ../compiler/interntl.zo ../compiler/labels.zo ../compiler/misc.zo \
    ../linker/symtable.zo ../linker/patch.zo ../compiler/instruct.zo \
    ../compiler/buffcode.zo ../compiler/emitcode.zo ../compiler/builtins.zo \
    ../compiler/opcodes.zo 
pr_value.zo: ../compiler/modules.zo ../compiler/const.zo \
    ../compiler/misc.zo fmt_type.zo ../linker/symtable.zo \
    ../compiler/globals.zo ../compiler/builtins.zo ../compiler/types.zo 
provide.zo: ../compiler/prim.zo ../compiler/modules.zo \
    ../compiler/config.zi ../compiler/const.zo ../compiler/misc.zo \
    ../compiler/globals.zo 
topinit.zo: meta.zi ../compiler/typing.zo toplevel.zi \
    ../compiler/modules.zo ../compiler/interntl.zo ../compiler/config.zi \
    ../compiler/misc.zo version.zo ../linker/symtable.zo 
toplevel.zo: toplevel.zi meta.zi ../compiler/modules.zo \
    ../compiler/emit_phr.zo ../compiler/misc.zo ../compiler/event.zo \
    ../linker/symtable.zo ../linker/patch.zo ../compiler/globals.zo \
    ../compiler/builtins.zo pr_value.zo ../compiler/location.zi \
    ../compiler/interntl.zo ../compiler/const.zo ../compiler/compiler.zo \
    load_phr.zo do_phr.zo ../compiler/types.zo ../compiler/opcodes.zo 
topmain.zo: ../compiler/location.zi ../compiler/interntl.zo \
    ../compiler/config.zi ../compiler/compiler.zo ../compiler/misc.zo \
    do_phr.zo 
version.zo: ../compiler/interntl.zo 
