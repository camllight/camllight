### Configuration section

# Which C compiler to use.
# Performance is often improved if you use gcc 2.x instead of cc.
CC=gcc

# Additional options to $(CC).
# If you are using gcc, add -fno-defer-pop.
# This option circumvents a gcc bug on some platforms (680x0, 80386).
# If you are using Linux with libc6 (RedHat 5, Debian 2), add -D__FAVOR_BSD
# This option avoids signal-related problems.
OPTS=-fno-defer-pop -D__FAVOR_BSD

# Extra libraries that have to be linked with the runtime system.
# The math library "-lm" is linked by default.
# On most machines, nothing else is needed.
# Under Solaris: -lsocket -lnsl
LIBS=

# How to call the C preprocessor on a file that does not have the .c extension.
# That's /lib/cpp on most machines, sometimes /usr/bin/cpp,
# and /usr/ccs/lib/cpp under Solaris.
# The -P option suppresses the generation of "# linenum" directives,
# which are not understood by Caml Light.
# The -Dunix option ensures that the symbol "unix" is defined --
# not all Unix C preprocessors define it.
# If your cpp is too fussy, make tools/clprepro and use this:
# CPP=../../src/tools/clprepro -Dunix
CPP=/lib/cpp -P -traditional -Dunix

# The directory where public executables will be installed
BINDIR=/usr/local/bin

# The directory where the Caml Light standard library will be installed
LIBDIR=/usr/local/lib/caml-light

# The manual section where the manual pages will be installed
MANEXT=1

# The directory where the manual pages will be installed
MANDIR=/usr/local/man/man$(MANEXT)

# Some "make"s need this to ensure that they call the Bourne shell,
# not the C shell. Seems harmless on most other "make"s.
# Try removing this line if you run into trouble.
SHELL=/bin/sh

### End of configuration section

SUBDIRS=runtime launch lib compiler linker librar toplevel lex yacc tools

# Configure the system
configure:
	cd ../config; sh autoconf "$(CC) $(OPTS) $(LIBS)"

# Build the system for the first time
world:
	cd runtime; make CC="$(CC)" OPTS="$(OPTS)" LIBS="$(LIBS)" all
	cp runtime/camlrun .
	cd yacc; make CC="$(CC)" OPTS="$(OPTS)" all
	cp yacc/camlyacc .
	cd lib; make CPP="$(CPP)" all
	cd compiler; make CPP="$(CPP)" all
	cd linker; make CPP="$(CPP)" all
	cd librar; make CPP="$(CPP)" all
	cd lex; make CPP="$(CPP)" all
	cd toplevel; make CPP="$(CPP)" all
	cd launch; make LIBDIR=$(LIBDIR) BINDIR=$(BINDIR) \
                           CC="$(CC)" OPTS="$(OPTS)" LIBS="$(LIBS)" all
	@ echo "Let's test quickly the toplevel system..."
	(echo "1+2;;";                                                        \
         echo "let rec fib n = if n < 2 then 1 else fib(n-1)+fib(n-2);;";     \
         echo "fib 20;;") | ./camlrun toplevel/camltop -stdlib lib
	@ echo "Is that 10946 on the line above? Good."
	@ echo "The Caml Light system is up and running."

# Rebuild the system (bootstrap)
bootstrap: backup promote again compare

# Save a copy of the current compiler
backup:
	sh tools/backup camlrun camlcomp camllink camllibr camllex 

# Make the newly compiled compiler the current compiler
promote:
	cp compiler/camlcomp linker/camllink librar/camllibr lex/camllex .

# Recompile all Caml code from scratch
again:
	cd lib; make CPP="$(CPP)" clean all
	cd compiler; make CPP="$(CPP)" clean all
	cd linker; make CPP="$(CPP)" clean all
	cd librar; make CPP="$(CPP)" clean all
	cd lex; make CPP="$(CPP)" clean all
	cd toplevel; make CPP="$(CPP)" clean all

# Compare the current compiler with the newly compiled one
compare:
	@sh -c ' \
        if cmp camlcomp compiler/camlcomp \
        && cmp camllink linker/camllink \
        && cmp camllibr librar/camllibr \
        && cmp camllex lex/camllex; \
        then echo "The Caml Light system has successfully recompiled itself.";\
        else echo "Hmph. Better do one more bootstrapping cycle."; \
        fi'

# Restore the latest backup copy of the compiler
restore:
	sh tools/restore camlrun camlcomp camllink camllibr camllex 

# Compile all Caml code
compile:
	cd lib; make CPP="$(CPP)" all
	cd compiler; make CPP="$(CPP)" all
	cd linker; make CPP="$(CPP)" all
	cd librar; make CPP="$(CPP)" all
	cd lex; make CPP="$(CPP)" all
	cd toplevel; make CPP="$(CPP)" all

# Install the Caml Light system
install:
	if test -d $(BINDIR); then : ; else mkdir $(BINDIR); fi
	if test -d $(LIBDIR); then : ; else mkdir $(LIBDIR); fi
	cd runtime; make BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) install
	cd launch; make BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) install
	cd lib; make BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) install
	cd compiler; make BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) install
	cd linker; make BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) install
	cd librar; make BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) install
	cd toplevel; make BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) install
	cd lex; make BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) install
	cd yacc; make BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) install
	cd man; make MANDIR=$(MANDIR) MANEXT=$(MANEXT) install
	cp camlmsgs.txt $(LIBDIR)

# Remove the Caml Light system after installation
uninstall:
	rm -rf $(LIBDIR)
	rm -f $(BINDIR)/camlrun $(BINDIR)/camlc $(BINDIR)/camllight
	rm -f $(BINDIR)/camlyacc $(BINDIR)/camllex $(BINDIR)/camlmktop

# Remove all generated files
clean:
	for d in $(SUBDIRS); \
	do (cd $$d; make clean); \
        done

# Rebuild the dependencies in all Makefiles
depend:
	for d in $(SUBDIRS); \
	do (cd $$d; make depend); \
        done
