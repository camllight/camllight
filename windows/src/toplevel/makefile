# Makefile for the Caml Light toplevel.

OUTPUT=camltop provide expunge

INCLUDES=-I ../compiler -I ../linker

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
     version.zo main.zo

PERVASIVES=baltree bool char eq exc fchar filename float format fstring fvect \
    gc genlex hashtbl int io iparsing lexing list map obj pair parsing \
    printexc printf queue random ref set sort stack stream string \
    toplevel vect graphics \
    big_int fnat nat num ratio arstatus

SPECIALS=sys

CAMLCOMP=..\camlrun ..\camlcomp
CAMLRUN=..\camlrun
CAMLLINK=..\camlrun ..\camllink
CAMLLIBR=..\camlrun ..\camllibr
CAMLLEX=..\camlrun ..\camllex
CAMLYACC=..\camlyacc -s
CPP=cl -EP -Dmsdos -Uunix
PERL=perl
COMPFLAGS=-stdlib ../lib -W -O fast $(INCLUDES)
LINKFLAGS=-stdlib ../lib -g $(INCLUDES)
GENSOURCES=pr_value.ml

all: camltop

camltop: toplib.zo provide expunge
	$(CAMLRUN) provide -stdlib ../lib @<< > required
$(PERVASIVES)
<<
	$(CAMLLINK) $(LINKFLAGS) -o camltop1 -require required stdlib.zo toplib.zo
	$(CAMLRUN) expunge camltop1 camltop @<<
$(PERVASIVES) $(SPECIALS)
<<
	del camltop1
	del required
	copy toplevel.zi ..\lib

toplib.zo: $(OBJS)
	$(CAMLLIBR) $(INCLUDES) -o toplib.zo @<<
$(EXTERNOBJS) $(OBJS)
<<

expunge: expunge.zo
	$(CAMLLINK) $(LINKFLAGS) -o expunge stdlib.zo readword.zo expunge.zo

provide: provide.zo
	$(CAMLLINK) $(LINKFLAGS) -o provide @<<
stdlib.zo config.zo misc.zo interntl.zo modules.zo provide.zo
<<

pr_value.ml: pr_value.mlp
	$(CPP) -DPRINT_NUMS pr_value.mlp > pr_value.ml

install:
	copy camltop $(LIBDIR)
	copy provide $(LIBDIR)
	copy expunge $(LIBDIR)
	copy toplib.zo $(LIBDIR)

!include "../tools/make-ml.inc"

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
do_phr.zo: pr_value.zo meta.zi load_phr.zo fmt_type.zo 
load_phr.zo: pr_value.zo meta.zi 
main.zo: meta.zi toplevel.zi version.zo do_phr.zo 
pr_value.zo: fmt_type.zo 
toplevel.zo: toplevel.zi meta.zi pr_value.zo load_phr.zo do_phr.zo 
