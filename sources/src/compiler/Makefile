# Makefile for the Caml Light compiler.

CAMLCOMP=../camlrun ../camlcomp -stdlib ../lib
CAMLLINK=../camlrun ../camllink -stdlib ../lib
CAMLLEX=../camlrun ../camllex
CAMLYACC=../camlyacc -s
CPP=/lib/cpp -P -Dunix

COMPFLAGS=-W -O fast
LINKFLAGS=-g

OBJS=config.zo misc.zo interntl.zo const.zo prim.zo lambda.zo globals.zo \
 location.zo syntax.zo \
 modules.zo builtins.zo types.zo \
 pr_type.zo error.zo typing.zo ty_decl.zo pr_decl.zo ty_intf.zo \
 tr_env.zo event.zo clauses.zo matching.zo trstream.zo front.zo \
 instruct.zo back.zo opcodes.zo prim_opc.zo buffcode.zo labels.zo reloc.zo \
 emitcode.zo emit_phr.zo \
 primdecl.zo lexer.zo par_aux.zo parser.zo compiler.zo \
 version.zo main.zo

GENSOURCES=lexer.ml parser.ml parser.mli opcodes.ml config.ml \
  location.ml version.ml

all: camlcomp

camlcomp: $(OBJS)
	$(CAMLLINK) $(LINKFLAGS) -o camlcomp stdlib.zo $(OBJS)
        
clean:
	rm -f *.zi *.zo camlcomp
	rm -f $(GENSOURCES) lexer.mll

install:
	cp camlcomp $(LIBDIR)/camlcomp

opcodes.ml: ../runtime/instruct.h
	sed -n -e '/^enum/p' -e 's/,//' -e '/^  /p' ../runtime/instruct.h | \
        awk -f ../tools/make-opcodes > opcodes.ml

lexer.mll: lexer.mlp
	$(CPP) lexer.mlp > lexer.mll

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

parser.ml parser.mli: parser.mly
	$(CAMLYACC) parser.mly

.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo .mlp

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.mlp.ml:
	@rm -f $@
	$(CPP) $< > $@
	@chmod a-w $@

depend: $(GENSOURCES)
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
         ../tools/camldep *.mli *.ml) > Makefile
	rm Makefile.bak

location.ml: location.mlp
config.ml: config.mlp
version.ml: version.mlp ../version.h

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
lexer.zi: parser.zi 
parser.zi: syntax.zo 
back.zo: prim.zo lambda.zo instruct.zo misc.zo const.zo 
buffcode.zo: error.zo 
builtins.zo: modules.zo globals.zo const.zo 
clauses.zo: lambda.zo globals.zo types.zo syntax.zo const.zo location.zi \
    error.zo misc.zo 
compiler.zo: lexer.zi ty_decl.zo syntax.zo front.zo error.zo misc.zo \
    parser.zi interntl.zo back.zo emit_phr.zo modules.zo location.zi \
    pr_decl.zo typing.zo ty_intf.zo 
config.zo: config.zi 
const.zo: misc.zo 
emit_phr.zo: lambda.zo event.zo emitcode.zo labels.zo instruct.zo reloc.zo \
    buffcode.zo 
emitcode.zo: lambda.zo event.zo labels.zo reloc.zo config.zi const.zo \
    prim.zo prim_opc.zo misc.zo instruct.zo opcodes.zo buffcode.zo 
error.zo: interntl.zo pr_type.zo globals.zo types.zo syntax.zo const.zo \
    location.zi misc.zo 
event.zo: modules.zo location.zi lambda.zo syntax.zo 
front.zo: lambda.zo event.zo trstream.zo globals.zo types.zo syntax.zo \
    const.zo prim.zo modules.zo error.zo builtins.zo matching.zo misc.zo \
    tr_env.zo 
globals.zo: prim.zo const.zo 
instruct.zo: prim.zo lambda.zo const.zo 
interntl.zo: misc.zo 
labels.zo: instruct.zo misc.zo buffcode.zo 
lambda.zo: prim.zo globals.zo const.zo 
lexer.zo: lexer.zi parser.zi 
location.zo: location.zi interntl.zo config.zi 
main.zo: interntl.zo event.zo version.zo config.zi modules.zo typing.zo \
    compiler.zo misc.zo 
matching.zo: clauses.zo lambda.zo globals.zo types.zo syntax.zo const.zo \
    prim.zo location.zi builtins.zo error.zo misc.zo 
modules.zo: interntl.zo globals.zo misc.zo const.zo 
par_aux.zo: modules.zo location.zi error.zo builtins.zo globals.zo misc.zo \
    syntax.zo const.zo 
parser.zo: parser.zi primdecl.zo par_aux.zo builtins.zo globals.zo \
    syntax.zo const.zo 
pr_decl.zo: pr_type.zo globals.zo misc.zo const.zo 
pr_type.zo: modules.zo globals.zo types.zo const.zo 
prim.zo: const.zo 
prim_opc.zo: prim.zo misc.zo opcodes.zo 
primdecl.zo: prim.zo globals.zo 
reloc.zo: buffcode.zo const.zo 
syntax.zo: location.zi globals.zo const.zo 
tr_env.zo: prim.zo lambda.zo builtins.zo error.zo globals.zo misc.zo \
    syntax.zo const.zo 
trstream.zo: prim.zo lambda.zo matching.zo tr_env.zo syntax.zo const.zo 
ty_decl.zo: globals.zo types.zo syntax.zo const.zo modules.zo builtins.zo \
    error.zo typing.zo 
ty_intf.zo: modules.zo error.zo ty_decl.zo globals.zo types.zo const.zo 
types.zo: modules.zo globals.zo misc.zo const.zo 
typing.zo: modules.zo error.zo builtins.zo globals.zo misc.zo types.zo \
    syntax.zo const.zo 
typing.old.zo: modules.zo error.zo builtins.zo globals.zo misc.zo types.zo \
    syntax.zo const.zo 
version.zo: interntl.zo 
