CAMLCOMP	=camlc -c
CAMLLEX		=camllex
CAMLYACC	=camlyacc
CAMLTEX		=../tools/caml-tex-format
CAMLDETEX	=../tools/caml-tex-extract
COMPFLAGS	=-I $(UNIXLIBDIR) -I $(COMPLIBDIR) $(DEBUG) -W
LINKFLAGS	=-I $(COMPLIBDIR) -I $(COMPLIBDIR) -g
YACCFLAGS	=# -v


DEBUG=	-g
NAME=	camlsearch

COMPOBJS= config.zo misc.zo interntl.zo const.zo prim.zo globals.zo \
        location.zo error.zo modules.zo builtins.zo types.zo
OBJS=	general.zo comparisons.zo directory_and_file.zo type_parser.zo \
	type_lexer.zo primitives.zo type_for_matching.zo \
	compiler_dependant_stuff.zo operations_on_types.zo output.zo \
	matching_on_bases.zo matching.zo command_line.zo normalization.zo \
	myTypes.zo TypeRewrite.zo LC_unify.zo Equal.zo camlsearch.zo
TEXOBJ= external_type.mli.f.tex type_for_matching.mli.f.tex
SOURCES=	*.ml *.mli *.mll *.mly Makefile

all:	$(NAME)

#					Crée l'executable
$(NAME):	$(OBJS)
	camlc -custom -g $(LINKFLAGS) -o $(NAME) $(COMPOBJS) $(OBJS) $(UNIXLIBDIR)/libunix.a

#					Formate pour TeX
report: $(TEXOBJ)


#					Install CamlSearch
install: $(NAME)
	cp $(NAME) $(INSTALLDIR)
	chmod 755 $(INSTALLDIR)/$(NAME)
#					Efface les fichiers inutiles
clean:
	rm -f $(NAME)
	rm -f *.zi *.zix *.zo *.f.tex *~

#					Tags pour Emacs
tags:
	mletags *.mli

#					Crée liste des dépendances
depend:
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
         ../../../src/tools/camldep *.mli *.ml) > Makefile

#					Comment compiler
.SUFFIXES :
#.SUFFIXES : .ml.tex .mli.tex .tex .f.tex .mll .mly .mli .ml .zi .zo .c .o
.SUFFIXES : .ml.tex .tex .f.tex .mll .mly .mli .ml .zi .zo .c .o .mli.tex

.tex.f.tex:
	$(CAMLTEX) $<

.mll.zo:
	$(CAMLLEX) $<
	$(CAMLCOMP) $(COMPFLAGS) $*.ml
	rm $*.ml

.mly.zi:
	$(CAMLYACC) $(YACCFLAGS) $<
	$(CAMLCOMP) $(COMPFLAGS) $*.mli $*.ml
	rm $*.mli $*.ml

.mly.zo:
	$(CAMLYACC) $(YACCFLAGS) $<
	$(CAMLCOMP) $(COMPFLAGS) $*.mli $*.ml
	rm $*.mli $*.ml

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.c.o:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.tex.zo:
	$(CAMLDETEX) $<
	$(CAMLCOMP) $(COMPFLAGS) $*.ml

.mli.tex.zi:
	$(CAMLDETEX) $<
	$(CAMLCOMP) $(COMPFLAGS) $*.mli

.mli.tex.mli:
	$(CAMLDETEX) $<

.ml.tex.ml:
	$(CAMLDETEX) $<

#					Dépendances
type_parser.zo:	external_type.zi
type_parser.zi: external_type.zi
type_lexer.zo: type_parser.zi
myTypeParser.zo:
myTypeParser.zi:
myLexer.zo:	myTypeParser.zi

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
Equal.zi: external_type.zi myTypes.zo 
LC_unify.zi: myTypes.zo 
TypeRewrite.zi: myTypes.zo 
command_line.zi: external_type.zi 
compiler_dependant_stuff.zi: external_type.zi 
matching.zi: type_for_matching.zi 
matching_on_bases.zi: type_for_matching.zi 
normalization.zi: external_type.zi type_for_matching.zi 
operations_on_types.zi: type_for_matching.zi comparisons.zi 
output.zi: external_type.zi 
Equal.zo: Equal.zi TypeRewrite.zi LC_unify.zi myTypes.zo output.zi 
LC_unify.zo: LC_unify.zi myTypes.zo 
TypeRewrite.zo: TypeRewrite.zi myTypes.zo 
camlsearch.zo: camlsearch.zi TypeRewrite.zi Equal.zi \
    compiler_dependant_stuff.zi normalization.zi directory_and_file.zi \
    general.zi output.zi matching.zi myTypes.zo command_line.zi 
command_line.zo: command_line.zi general.zi 
comparisons.zo: comparisons.zi 
compiler_dependant_stuff.zo: compiler_dependant_stuff.zi external_type.zi 
directory_and_file.zo: directory_and_file.zi 
general.zo: general.zi 
matching.zo: matching.zi operations_on_types.zi type_for_matching.zi \
    primitives.zi general.zi matching_on_bases.zi comparisons.zi 
matching_on_bases.zo: matching_on_bases.zi operations_on_types.zi \
    type_for_matching.zi 
myTypes.zo: external_type.zi 
normalization.zo: normalization.zi external_type.zi operations_on_types.zi \
    type_for_matching.zi 
operations_on_types.zo: operations_on_types.zi type_for_matching.zi \
    comparisons.zi 
output.zo: output.zi external_type.zi 
primitives.zo: primitives.zi 
type_for_matching.zo: type_for_matching.zi 
