CC		= gcc
OPTS		= -DANSI
CAMLCOMP	= camlc
CAMLLEX		= camllex
CAMLYACC	= camlyacc -v
CFLAGS		= $(DEBUG) -Wall $(OPTS)
INCLUDES	= -I ../compiler -I ../linker
COMPFLAGS	= -c $(DEBUG) $(INCLUDES)
LINKFLAGS	= -g $(INCLUDES) -ccopt -Wall
YACCFLAGS	=# -v
LIBS		= -lunix

DEBUG=	-g
NAME=	camldebug

EXTERNOBJS = \
	config.zo globals.zo misc.zo ../runtime/io.o \
	symtable.zo modules.zo builtins.zo \
	types.zo pr_type.zo \
	tr_env.zo
OBJS =	debugger_config.zo constants.zo primitives.zo unix_tools.zo \
	modules2.zo \
	parameters.zo \
	lexer.zo \
	interruption.zo \
	input_handling.zo \
	communication.zo communication.o \
	value.zo value.o \
	source.zo \
	pr_value.zo \
	checkpoints.zo \
	events.zo \
	breakpoints.zo \
	trap_barrier.zo \
	history.zo \
	program_loading.zo \
	time_travel.zo \
	program_management.zo \
	frames.zo \
	variables.zo \
	pattern_matching.zo \
	show_source.zo \
	show_information.zo \
	parser.zo \
	command_line_interpreter.zo \
	version.zo \
	main.zo

all:	$(NAME)

#					Crée l'executable
$(NAME):	$(OBJS)
	camlc -g -custom $(LINKFLAGS) -o $(NAME) unix.zo \
		$(EXTERNOBJS) $(OBJS) $(LIBS)

#					Efface les fichiers inutiles

dump_event: constants.zo dump_event.zo
	camlc -g $(LINKFLAGS) -o dump_event symtable.zo constants.zo \
		information.zo dump_event.zo

clean:
#	rm -f $(NAME) *~
	rm -f *.zi *.zix *.zo *.o
	rm -f constants.ml


install:
	cp camldebug $(BINDIR)
	strip $(BINDIR)/camlrun

#					Tags pour Emacs
tags:
	mletags *.mli *.ml ../compiler/*.mli ../compiler/*.ml ../linker/*.ml
	etags -a *.[ch] ../runtime/*.[ch]

#					Crée liste des dépendances
depend:
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
         (camldep *.mli *.ml; gcc -MM $(OPTS) *.c)) > Makefile

#					Comment compiler
.SUFFIXES :
.SUFFIXES : .mll .mly .mli .ml .zi .zo .c .o

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

.ml.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.c.o:
	$(CC) $(COMPFLAGS) $<

constants.zo constants.zi: constants.c
	$(CC) -o constants constants.c
	constants > constants.ml
	$(CAMLCOMP) $(COMPFLAGS) constants.ml

parser.zi: primitives.zi input_handling.zi parser_aux.zi
parser.zo: primitives.zi input_handling.zi parser_aux.zi
lexer.zo: parser.zi

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
breakpoints.zi: primitives.zi 
checkpoints.zi: primitives.zi communication.zi 
communication.zi: primitives.zi value.zi 
events.zi: primitives.zi 
frames.zi: primitives.zi 
input_handling.zi: primitives.zi 
parser_aux.zi: primitives.zi 
pattern_matching.zi: parser_aux.zi value.zi 
program_loading.zi: primitives.zi 
source.zi: primitives.zi 
time_travel.zi: primitives.zi communication.zi 
trap_barrier.zi: checkpoints.zi 
variables.zi: primitives.zi value.zi 
breakpoints.zo: breakpoints.zi source.zi primitives.zi checkpoints.zi \
    constants.zo communication.zi interruption.zi 
checkpoints.zo: checkpoints.zi primitives.zi communication.zi 
command_line_interpreter.zo: command_line_interpreter.zi breakpoints.zi \
    time_travel.zi show_information.zo parameters.zi primitives.zi value.zi \
    program_loading.zi program_management.zi checkpoints.zi events.zi \
    debugger_config.zi variables.zi unix_tools.zi pattern_matching.zi \
    frames.zi source.zi pr_value.zo parser_aux.zi input_handling.zi \
    communication.zi modules2.zi 
communication.zo: communication.zi primitives.zi input_handling.zi \
    unix_tools.zi 
debugger_config.zo: debugger_config.zi 
events.zo: events.zi checkpoints.zi primitives.zi communication.zi 
frames.zo: frames.zi events.zi checkpoints.zi primitives.zi \
    communication.zi 
history.zo: history.zi checkpoints.zi 
input_handling.zo: input_handling.zi primitives.zi 
interruption.zo: interruption.zi 
main.zo: show_information.zo parameters.zi time_travel.zi primitives.zi \
    program_loading.zi program_management.zi checkpoints.zi \
    command_line_interpreter.zi debugger_config.zi frames.zi \
    interruption.zi input_handling.zi version.zo communication.zi \
    modules2.zi 
modules2.zo: modules2.zi 
parameters.zo: parameters.zi primitives.zi 
pattern_matching.zo: pattern_matching.zi primitives.zi parser_aux.zi \
    value.zi 
pr_value.zo: value.zi 
primitives.zo: primitives.zi 
program_loading.zo: program_loading.zi parameters.zi primitives.zi \
    debugger_config.zi unix_tools.zi input_handling.zi 
program_management.zo: program_management.zi time_travel.zi parameters.zi \
    primitives.zi program_loading.zi input_handling.zi checkpoints.zi \
    events.zi debugger_config.zi constants.zo history.zi unix_tools.zi \
    communication.zi modules2.zi 
show_information.zo: breakpoints.zi show_source.zi events.zi checkpoints.zi \
    pr_value.zo primitives.zi frames.zi communication.zi 
show_source.zo: show_source.zi source.zi primitives.zi parameters.zi \
    debugger_config.zi 
source.zo: source.zi primitives.zi 
time_travel.zo: time_travel.zi breakpoints.zi primitives.zi \
    program_loading.zi input_handling.zi events.zi checkpoints.zi \
    debugger_config.zi trap_barrier.zi communication.zi interruption.zi 
trap_barrier.zo: trap_barrier.zi checkpoints.zi communication.zi \
    interruption.zi 
unix_tools.zo: unix_tools.zi primitives.zi 
value.zo: value.zi constants.zo communication.zi 
variables.zo: variables.zi primitives.zi value.zi frames.zi \
    communication.zi 
communication.o : communication.c ../runtime/mlvalues.h ../runtime/config.h \
  ../runtime/../../config/m.h ../runtime/../../config/s.h ../runtime/fail.h ../runtime/io.h \
  ../runtime/alloc.h 
constants.o : constants.c ../runtime/mlvalues.h ../runtime/config.h ../runtime/../../config/m.h \
  ../runtime/../../config/s.h ../runtime/exec.h ../runtime/instruct.h 
value.o : value.c ../runtime/mlvalues.h ../runtime/config.h ../runtime/../../config/m.h \
  ../runtime/../../config/s.h ../runtime/fail.h ../runtime/alloc.h 
