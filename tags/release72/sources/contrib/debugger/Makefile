# Makefile for the Caml Light debugger

# These options are overriden when called from ../Makefile
CC=cc
OPTS=
LIBDIR=/usr/local/lib/caml-light
CPP=/lib/cpp -P

# Compilation options
CAMLC=camlc
CAMLLEX=camllex
CAMLYACC=camlyacc
INCLUDES=-I ../libunix -I ../../src/compiler -I ../../src/linker -I ../../src/toplevel
COMPFLAGS=-W $(INCLUDES)
LINKFLAGS=$(INCLUDES)
CFLAGS=$(OPTS) -O -I../../src/runtime

EXTERNOBJS = \
	config.zo globals.zo misc.zo interntl.zo \
	symtable.zo modules.zo builtins.zo \
	types.zo fmt_type.zo \
	tr_env.zo 
OBJS =	debugger_config.zo constants.zo primitives.zo unix_tools.zo \
	modules2.zo \
	parameters.zo \
	lexer.zo \
	interruption.zo \
	input_handling.zo \
	communication.zo \
	value.zo \
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

COBJS=communication.o value.o

GENFILES=constants.ml lexer.ml parser.ml parser.mli main.ml version.ml

all: camldebug

camldebug: $(OBJS) $(COBJS)
	$(CAMLC) $(LINKFLAGS) -custom -o camldebug \
          unix.zo $(EXTERNOBJS) $(OBJS) $(COBJS) ../libunix/libunix.a

clean:
	rm -f camldebug *.zi *.zo *.o
	rm -f $(GENFILES)

install:
	cp camldebug $(BINDIR)/camldebug

.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo .c .o

.mli.zi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.zo:
	$(CAMLC) -c $(COMPFLAGS) $<

.c.o:
	$(CC) $(CFLAGS) -c $<

constants.ml: constants.c
	@rm -f constants.ml
	$(CC) $(CFLAGS) -o constants constants.c
	./constants > constants.ml
	rm -f constants
	@chmod -w constants.ml

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

parser.ml parser.mli: parser.mly
	$(CAMLYACC) parser.mly

main.ml: main.mlp
	@rm -f main.ml
	sed -e 's|LIBDIR|$(LIBDIR)|' main.mlp > main.ml
	@chmod -w main.ml

version.ml: version.mlp ../../src/version.h
	@rm -f version.ml
	$(CPP) -I../../src version.mlp > version.ml
	@chmod -w version.ml

depend: $(GENFILES)
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
         ../../src/tools/camldep *.mli *.ml; \
         gcc -MM $(OPTS) -I../../src/runtime *.c) > Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
checkpoints.zi: primitives.zi communication.zi 
communication.zi: primitives.zi value.zi 
input_handling.zi: primitives.zi 
parser.zi: parser_aux.zi 
pattern_matching.zi: parser_aux.zi value.zi 
pr_value.zi: value.zi 
program_loading.zi: primitives.zi 
time_travel.zi: primitives.zi 
variables.zi: value.zi 
breakpoints.zo: breakpoints.zi source.zi primitives.zi checkpoints.zi \
    constants.zo communication.zi interruption.zi 
checkpoints.zo: checkpoints.zi primitives.zi communication.zi 
command_line_interpreter.zo: command_line_interpreter.zi breakpoints.zi \
    show_source.zi time_travel.zi show_information.zi parameters.zi \
    primitives.zi program_loading.zi program_management.zi checkpoints.zi \
    events.zi debugger_config.zi lexer.zo variables.zi history.zi \
    pattern_matching.zi frames.zi unix_tools.zi source.zi pr_value.zi \
    parser_aux.zi parser.zi input_handling.zi communication.zi 
communication.zo: communication.zi primitives.zi value.zi input_handling.zi 
debugger_config.zo: debugger_config.zi 
events.zo: events.zi checkpoints.zi primitives.zi 
frames.zo: frames.zi events.zi checkpoints.zi communication.zi 
history.zo: history.zi checkpoints.zi debugger_config.zi primitives.zi 
input_handling.zo: input_handling.zi primitives.zi lexer.zo 
interruption.zo: interruption.zi 
lexer.zo: primitives.zi parser.zi 
main.zo: show_information.zi parameters.zi time_travel.zi input_handling.zi \
    version.zo program_management.zi command_line_interpreter.zi \
    checkpoints.zi debugger_config.zi frames.zi interruption.zi 
modules2.zo: modules2.zi 
parameters.zo: parameters.zi 
parser.zo: parser.zi parser_aux.zi input_handling.zi 
pattern_matching.zo: pattern_matching.zi parser_aux.zi primitives.zi \
    value.zi 
pr_value.zo: pr_value.zi value.zi 
primitives.zo: primitives.zi 
program_loading.zo: program_loading.zi parameters.zi primitives.zi \
    debugger_config.zi unix_tools.zi input_handling.zi 
program_management.zo: program_management.zi time_travel.zi parameters.zi \
    primitives.zi program_loading.zi events.zi debugger_config.zi \
    history.zi unix_tools.zi input_handling.zi constants.zo \
    communication.zi modules2.zi 
show_information.zo: show_information.zi breakpoints.zi show_source.zi \
    events.zi checkpoints.zi pr_value.zi frames.zi communication.zi 
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
variables.zo: variables.zi value.zi frames.zi communication.zi 
communication.o: communication.c \
 ../../src/runtime/../runtime/mlvalues.h \
 ../../src/runtime/../runtime/config.h \
 ../../src/runtime/../runtime/../../config/m.h \
 ../../src/runtime/../runtime/../../config/s.h \
 ../../src/runtime/../runtime/misc.h \
 ../../src/runtime/../runtime/fail.h ../../src/runtime/../runtime/io.h \
 ../../src/runtime/../runtime/alloc.h
constants.o: constants.c
value.o: value.c ../../src/runtime/../runtime/mlvalues.h \
 ../../src/runtime/../runtime/config.h \
 ../../src/runtime/../runtime/../../config/m.h \
 ../../src/runtime/../runtime/../../config/s.h \
 ../../src/runtime/../runtime/misc.h \
 ../../src/runtime/../runtime/fail.h \
 ../../src/runtime/../runtime/alloc.h
