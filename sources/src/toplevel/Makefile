# Makefile for the Caml Light toplevel.

CAMLCOMP=../camlrun ../camlcomp
CAMLLINK=../camlrun ../camllink
CAMLLIBR=../camlrun ../camllibr
CAMLRUN=../camlrun
INCLUDES=-stdlib ../lib -I ../compiler -I ../linker
COMPFLAGS=-O fast $(INCLUDES)
LINKFLAGS=-g $(INCLUDES)

EXTERNOBJS=config.zo misc.zo \
    const.zo prim.zo instruct.zo lambda.zo \
    globals.zo location.zo syntax.zo modules.zo builtins.zo \
    types.zo pr_type.zo error.zo typing.zo ty_decl.zo pr_decl.zo \
    ty_intf.zo tr_env.zo event.zo clauses.zo match.zo trstream.zo front.zo \
    back.zo opcodes.zo prim_opc.zo buffcode.zo labels.zo \
    reloc.zo emitcode.zo emit_phr.zo \
    primdecl.zo lexer.zo par_aux.zo parser.zo compiler.zo \
    predef.zo prim_c.zo symtable.zo patch.zo tr_const.zo

OBJS=pr_value.zo load_phr.zo do_phr.zo toplevel.zo version.zo main.zo

PERVASIVES=baltree bool char eq exc fchar filename float fstring fvect \
    gc genlex hashtbl int io iparsing lexing list map obj pair parsing \
    printexc printf queue random ref set sort stack stream string \
    toplevel vect

SPECIALS=sys

GENSOURCES=version.ml

all: camltop

camltop: toplib.zo provide expunge
	$(CAMLRUN) ./provide -stdlib ../lib $(PERVASIVES) > required
	$(CAMLLINK) $(LINKFLAGS) -o camltop1 -require required \
                stdlib.zo toplib.zo
	echo $(PERVASIVES) $(SPECIALS) | $(CAMLRUN) ./expunge camltop1 camltop
	rm -f camltop1 required
	cp toplevel.zi ../lib

toplib.zo: $(OBJS)
	$(CAMLLIBR) -o toplib.zo $(INCLUDES) $(EXTERNOBJS) $(OBJS)

expunge: expunge.zo
	$(CAMLLINK) $(LINKFLAGS) -o expunge stdlib.zo readword.zo expunge.zo

provide: provide.zo
	$(CAMLLINK) $(LINKFLAGS) -o provide stdlib.zo \
                config.zo misc.zo modules.zo provide.zo

clean:
	rm -f *.zi *.zo camltop expunge provide
	rm -f $(GENSOURCES)

install:
	cp camltop $(LIBDIR)/camltop
	cp provide $(LIBDIR)/provide
	cp expunge $(LIBDIR)/expunge
	cp toplib.zo $(LIBDIR)/toplib.zo

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

depend: $(GENSOURCES)
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
         ../tools/camldep -I ../compiler -I ../linker *.mli *.ml) > Makefile
	rm Makefile.bak

version.ml: version.mlp ../version.h

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
do_phr.zo: pr_value.zo ../compiler/typing.zo meta.zi \
    ../compiler/location.zi ../compiler/modules.zo ../compiler/compiler.zo \
    ../compiler/const.zo ../compiler/misc.zo load_phr.zo \
    ../compiler/back.zo ../compiler/front.zo ../linker/symtable.zo \
    ../compiler/syntax.zo ../compiler/ty_decl.zo ../compiler/pr_type.zo 
expunge.zo: ../linker/symtable.zo ../linker/readword.zo \
    ../compiler/const.zo 
load_phr.zo: pr_value.zo ../compiler/reloc.zo meta.zi ../linker/tr_const.zo \
    ../compiler/labels.zo ../compiler/misc.zo ../linker/symtable.zo \
    ../linker/patch.zo ../compiler/instruct.zo ../compiler/buffcode.zo \
    ../compiler/emitcode.zo ../compiler/builtins.zo ../compiler/opcodes.zo 
main.zo: meta.zi toplevel.zi ../compiler/location.zi ../compiler/modules.zo \
    ../compiler/config.zi ../compiler/compiler.zo version.zo \
    ../compiler/misc.zo do_phr.zo ../linker/symtable.zo 
pr_value.zo: ../compiler/misc.zo ../linker/symtable.zo \
    ../compiler/pr_type.zo ../compiler/modules.zo ../compiler/builtins.zo \
    ../compiler/globals.zo ../compiler/types.zo ../compiler/const.zo 
provide.zo: ../compiler/prim.zo ../compiler/modules.zo \
    ../compiler/config.zi ../compiler/const.zo ../compiler/misc.zo \
    ../compiler/globals.zo 
toplevel.zo: toplevel.zi meta.zi ../compiler/modules.zo \
    ../compiler/emit_phr.zo ../compiler/misc.zo ../compiler/event.zo \
    ../linker/symtable.zo ../linker/patch.zo ../compiler/lexer.zi \
    ../compiler/builtins.zo ../compiler/globals.zo pr_value.zo \
    ../compiler/location.zi ../compiler/const.zo ../compiler/compiler.zo \
    load_phr.zo do_phr.zo ../compiler/instruct.zo ../compiler/types.zo \
    ../compiler/opcodes.zo 
