### System dependent configuration section

COMPLIBDIR=../../src/compiler

### End of system dependent configuration section

CAMLC=camlc
CAMLYACC=camlyacc
CAMLLEX=camllex
COMPFLAGS=-I $(COMPLIBDIR)
LINKFLAGS=-I $(COMPLIBDIR) -g

SRC = common.ml myTypes.ml TypeOfString.ml Search_isos.ml myLexer.mll \
    myTypeParser.mly TypeRewrite.ml LC_unify.ml Equal.ml module_iterator.ml \
    TypeOfString.mli TypeRewrite.mli LC_unify.mli Equal.mli \
    module_iterator.mli Search_isos.mli

IMPL = common.zo myTypes.zo myLexer.zo myTypeParser.zo TypeOfString.zo \
    module_iterator.zo TypeRewrite.zo LC_unify.zo Equal.zo Search_isos.zo

COMPMODS = config.zo misc.zo const.zo prim.zo globals.zo location.zo \
    syntax.zo errors.zo modules.zo builtins.zo types.zo pr_type.zo \
    ty_error.zo typing.zo par_aux.zo

all: search_isos

search_isos: $(IMPL)
	$(CAMLC) $(LINKFLAGS) -o search_isos -custom \
        unix.zo $(COMPMODS) $(IMPL) -lunix

clean: 
	rm -f search_isos
	rm -f *.z[io]

distrib:	
	tar -cvf - $(SRC) Makefile README | compress -c > search_isos.tar.Z

depend: $(SRC) myTypeParser.mli myTypeParser.ml myLexer.ml
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
	 camldep *.mli *.ml) > Makefile

myLexer.ml: myLexer.mll
	$(CAMLLEX) myLexer.mll

myTypeParser.ml myTypeParser.mli: myTypeParser.mly
	$(CAMLYACC) myTypeParser.mly

.SUFFIXES: .ml .mli .zo .zi

.mli.zi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.zo:
	$(CAMLC) -c $(COMPFLAGS) $<

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
Equal.zi: myTypes.zo 
LC_unify.zi: myTypes.zo 
TypeRewrite.zi: myTypes.zo 
Equal.zo: Equal.zi TypeRewrite.zi LC_unify.zi myTypes.zo 
LC_unify.zo: LC_unify.zi TypeRewrite.zi myTypes.zo 
Search_isos.zo: Search_isos.zi TypeRewrite.zi Equal.zi common.zo \
    module_iterator.zi TypeOfString.zi myTypes.zo 
TypeOfString.zo: TypeOfString.zi common.zo myTypeParser.zi myLexer.zo \
    myTypes.zo 
TypeRewrite.zo: TypeRewrite.zi common.zo myTypes.zo 
module_iterator.zo: module_iterator.zi 
myLexer.zo: common.zo myTypeParser.zi 
myTypeParser.zo: myTypeParser.zi 
