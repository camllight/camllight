# Overriden when called from ../Makefile
LIBDIR=/usr/local/lib/caml-light
BINDIR=/usr/local/bin

COMPLIBDIR=../../src/compiler
UNIXLIBDIR=../libunix
CAMLC=camlc
CAMLYACC=camlyacc
CAMLLEX=camllex
COMPFLAGS=-I $(COMPLIBDIR) -I $(UNIXLIBDIR)
LINKFLAGS=-I $(COMPLIBDIR) -I $(UNIXLIBDIR) -g

SRC = common.ml myTypes.ml TypeOfString.ml Search_isos.ml myLexer.mll \
    myTypeParser.mly TypeRewrite.ml LC_unify.ml Equal.ml module_iterator.ml \
    TypeOfString.mli TypeRewrite.mli LC_unify.mli Equal.mli \
    module_iterator.mli Search_isos.mli

IMPL = common.zo myTypes.zo myLexer.zo myTypeParser.zo TypeOfString.zo \
    module_iterator.zo TypeRewrite.zo LC_unify.zo Equal.zo Search_isos.zo

COMPMODS = config.zo misc.zo interntl.zo const.zo prim.zo globals.zo \
    location.zo syntax.zo modules.zo types.zo pr_type.zo error.zo builtins.zo \
    typing.zo par_aux.zo

GENFILES = myTypeParser.mli myTypeParser.ml myLexer.ml Search_isos.ml

all: search_isos

search_isos: $(IMPL)
	$(CAMLC) $(LINKFLAGS) -o search_isos -custom \
        unix.zo $(COMPMODS) $(IMPL) $(UNIXLIBDIR)/libunix.a

clean: 
	rm -f search_isos
	rm -f *.z[io] $(GENFILES)

install:
	cp search_isos $(BINDIR)/camlsearch

distrib:	
	tar -cvf - $(SRC) Makefile README | compress -c > search_isos.tar.Z

depend: $(GENFILES)
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
	 camldep *.mli *.ml) > Makefile

myLexer.ml: myLexer.mll
	$(CAMLLEX) myLexer.mll

myTypeParser.ml myTypeParser.mli: myTypeParser.mly
	$(CAMLYACC) myTypeParser.mly

Search_isos.ml: Search_isos.mlp
	@rm -f Search_isos.ml
	sed -e 's|LIBDIR|$(LIBDIR)|' Search_isos.mlp > Search_isos.ml
	@chmod -w Search_isos.ml

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
