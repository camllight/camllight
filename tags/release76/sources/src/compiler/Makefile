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
back.zo: prim.zo lambda.zo const.zo misc.zo instruct.zo 
buffcode.zo: error.zo 
builtins.zo: modules.zo globals.zo const.zo 
clauses.zo: lambda.zo error.zo location.zi const.zo misc.zo syntax.zo \
    globals.zo types.zo 
compiler.zo: pr_decl.zo typing.zo parser.zi error.zo modules.zo location.zi \
    interntl.zo emit_phr.zo misc.zo back.zo front.zo ty_intf.zo syntax.zo \
    lexer.zi ty_decl.zo 
config.zo: config.zi 
const.zo: misc.zo 
emit_phr.zo: event.zo reloc.zo lambda.zo emitcode.zo buffcode.zo \
    instruct.zo labels.zo 
emitcode.zo: prim.zo reloc.zo prim_opc.zo lambda.zo config.zi labels.zo \
    const.zo misc.zo event.zo instruct.zo buffcode.zo opcodes.zo 
error.zo: misc.zo syntax.zo pr_type.zo location.zi interntl.zo globals.zo \
    types.zo const.zo 
event.zo: syntax.zo lambda.zo modules.zo location.zi 
front.zo: trstream.zo prim.zo lambda.zo error.zo tr_env.zo matching.zo \
    modules.zo const.zo misc.zo event.zo syntax.zo globals.zo builtins.zo \
    types.zo 
globals.zo: prim.zo const.zo 
instruct.zo: prim.zo lambda.zo const.zo 
interntl.zo: misc.zo 
labels.zo: misc.zo buffcode.zo instruct.zo 
lambda.zo: prim.zo globals.zo const.zo 
lexer.zo: lexer.zi parser.zi 
location.zo: location.zi interntl.zo config.zi 
main.zo: typing.zo modules.zo interntl.zo config.zi compiler.zo misc.zo \
    version.zo event.zo 
matching.zo: clauses.zo prim.zo lambda.zo error.zo location.zi const.zo \
    misc.zo syntax.zo globals.zo builtins.zo types.zo 
modules.zo: misc.zo interntl.zo globals.zo const.zo 
par_aux.zo: error.zo modules.zo location.zi const.zo misc.zo syntax.zo \
    globals.zo builtins.zo 
parser.zo: parser.zi primdecl.zo par_aux.zo syntax.zo builtins.zo \
    globals.zo const.zo 
pr_decl.zo: misc.zo pr_type.zo globals.zo const.zo 
pr_type.zo: modules.zo globals.zo types.zo const.zo 
prim.zo: const.zo 
prim_opc.zo: misc.zo prim.zo opcodes.zo 
primdecl.zo: prim.zo globals.zo 
reloc.zo: buffcode.zo const.zo 
syntax.zo: location.zi globals.zo const.zo 
tr_env.zo: prim.zo lambda.zo error.zo const.zo misc.zo syntax.zo \
    builtins.zo globals.zo 
trstream.zo: prim.zo lambda.zo syntax.zo tr_env.zo matching.zo const.zo 
ty_decl.zo: typing.zo syntax.zo error.zo modules.zo builtins.zo globals.zo \
    types.zo const.zo 
ty_intf.zo: ty_decl.zo error.zo modules.zo globals.zo types.zo const.zo 
types.zo: misc.zo modules.zo globals.zo const.zo 
typing.zo: error.zo modules.zo const.zo misc.zo syntax.zo globals.zo \
    builtins.zo types.zo 
typing.old.zo: error.zo modules.zo const.zo misc.zo syntax.zo globals.zo \
    builtins.zo types.zo 
version.zo: interntl.zo 
