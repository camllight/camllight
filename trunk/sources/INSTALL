
               INSTALLING CAML LIGHT ON A UNIX SYSTEM


1- Edit the file src/Makefile. Change the variable definitions at
the beginning of the Makefile, to indicate which C compiler to use,
and where to install files. See the machine-specific hints at the end
of this file.

2- Configure the system. In the src/ subdirectory, do:

        make configure

This generates the two configuration files "m.h" and "s.h" in the
config/ subdirectory.  If something goes wrong during the make,
or if the generated "m.h" and "s.h" files cause errors later on, then
change to the config/ subdirectory, do

        cp m-templ.h m.h
        cp s-templ.h s.h

and edit "m.h" and "s.h" by hand, following the guidelines in the
comments.

3- From the src/ subdirectory, do:

        make world

This builds all components of Caml Light for the first time. It takes
about two minutes on a modern workstation. The "make" ends up with a
little self-test. Don't forget to check the results, as
indicated. This phase is fairly verbose; consider redirecting the
output to a file:

        make world > log.world 2>&1     # in sh
        make world >& log.world         # in csh

4- To be sure everything works well, you can try to bootstrap the
system --- that is, to recompile all Caml Light sources with the newly
created compiler. From the src/ subdirectory, do:

        make bootstrap

or, better:

        make bootstrap > log.bootstrap 2>&1     # in sh
        make bootstrap >& log.bootstrap         # in csh

This takes slightly less time than the "make world" phase. The "make
bootstrap" checks that the bytecode programs compiled with the new
compiler are identical to the bytecode programs compiled with the old
compiler. If this is the case, you can be pretty sure the Caml Light
system has been correctly compiled. Otherwise, this does not
necessarily means something went wrong. The best thing to do is to try
a second bootstrapping phase: just do "make bootstrap" again.  It will
either crash almost immediately, or re-re-compile everything correctly
and reach the fixpoint.

5- You can now install the Caml Light system. This will create the
following commands (in the directory set to BINDIR in src/Makefile):

        camllight       the interactive, toplevel-based system
        camlc           the batch compiler
        camlrun         the runtime system
        camlyacc        the parser generator
        camllex         the lexer generator
        camlmktop       a tool to make toplevel systems that integrate
                        user-defined C primitives

From the src/ directory, become superuser and do "make install".

6- The directory where camlrun resides must be in the PATH variable
for camlc and camllight to work properly. (Actually, camlc and
camllight are shell-scripts that call "camlrun" on various
bytecode files.) Hence, if you have installed camlrun in a
non-standard directory, be careful to add it to the PATH variable
before running camlc or camllight.

7- Now that the Caml Light compiler is installed, you can compile the
tools and libraries contained in the contrib/ directory: source-level
replay debugger, X Windows user interface toolkit, "tags" program for
Emacs, arbitrary-precision rational arithmetic, Unix system calls
interface, etc. The file contrib/INDEX gives a short description of
the packages contained in contrib/ (what they do, what they require).
The subdirectories of contrib/ also contain more information in README
and INSTALL files.  Read the descriptions and choose which packages
you need.

8- Edit the file contrib/Makefile to indicate which packages to
install, which C compiler to use, where to find external libraries on
your system, and where to install files.  It is highly recommended to
use the same C compiler used to compile the core system (the one
specified in src/Makefile).

9- Make sure you have not erased any of the files in src/ that have
been generated during the compilation of the core system.  If you
have, recompile the core system (step 3- above).

10- From the contrib/ subdirectory, do:

        make all

or, better,

        make all > log.all 2>&1     # in sh
        make all >& log.all         # in csh

11- You can now install the packages.  From the contrib/ directory,
become superuser and do "make install".

12- Installation is complete. Time to clean up.

        (cd src; make clean)
        (cd contrib; make clean)


IF SOMETHING GOES WRONG:

Read the "common problems" and "machine-specific hints" section at the
end of this file.

Check the files m.h and s.h in config/. Wrong endianness or alignment
constraints in m.h will immediately crash the bytecode interpreter.

If you get a "segmentation violation" signal, check the limits on the
stack size and data segment size (type "limit" under csh or
"ulimit -a" under bash). Make sure the limit on the stack size is
at least 2M.

Try recompiling the runtime system with optimizations turned off. The
runtime system contains some complex, atypical pieces of C code that
can uncover bugs in optimizing compilers. Alternatively, try another C
compiler (e.g. gcc instead of the vendor-supplied cc).

You can also build a debug version of the runtime system. Go to the
src/runtime/ directory and do "make camlrund". Then, copy camlrund to
../camlrun, and try again. This version of the runtime system contains
lots of assertions and sanity checks that could help you pinpoint the
problem.

If something goes wrong during the compilation of one of the packages
in contrib/, check the README and INSTALL files in the corresponding
directory for hints. If you really can't get one of the packages to
compile, remove it from the PACKAGES variable in contrib/Makefile and
go ahead with the others.


COMMON PROBLEMS:

* camlc or camllight complain that camlrun cannot be found. Make sure
that the directory containing camlrun is in your PATH (see point 6- above).

* The Makefiles assume that make execute commands by calling /bin/sh. They
won't work if /bin/csh is called instead. You may have to unset the SHELL
environment variable, or set it to /bin/sh.

* You can safely ignore the following warnings:

- ar or ranlib complains that fix_code.o has no symbol table.
  It's actually empty with some configurations.

- type clashes between enumeration types and integers. This is perfectly
  correct ANSI C.

* gcc 2.6.0 has been reported to generate incorrect code for the
runtime system in -O mode. Upgrade to 2.7.2 or turn -O off.

MACHINE-SPECIFIC HINTS:

* On HP 9000/700 machines under HP/UX 9.  Some versions of cc are
unable to compile correctly the runtime system (wrong code is
generated for (x - y) where x is a pointer and y an integer). This
causes "make world" to crash when compiling in src/lib.  Fix: use
another C compiler (gcc works fine).

* On DECstations 3000 under OSF1 3.0: "make configure" hangs while
testing asynchronous I/O. This may even hang your login shell as well.
Apparently, asynchronous I/O are severely buggy in the 3.0 kernel.
Fix: comment out lines 287-290 in config/autoconf and run "make configure"
again.

* On older versions of Linux, it has been reported that sed has a
non-standard syntax for scripts that causes some of the sed-scripts in
src/runtime/Makefile and src/linker/Makefile to fail during "make world".
(We did not experience this problem with the versions of Linux we
use.) You'll have to adapt the sed scripts, then do "make clean", then
do "make world" again. It is crucial to do "make clean", otherwise
incorrectly generated files will remain.

* On MIPS machines from MIPS Co.  Add "-systype bsd43" to OPTS.
Also, some versions of the cc compiler are reportedly unable to
compile src/runtime/interp.c ("as1: internal: unexpected opcode bcond06").
Either compile without optimizations (remove -O from OPTS in
src/Makefile) or use gcc.

* On some Next machines. cc pretends to be gcc but is not quite gcc.
If the compilation of src/runtime/interp.c causes syntax errors, insert
#undef __GNUC__ at the very beginning of src/runtime/misc.h.

* On SGI Indigo under IRIX 4.0. "ar" emits some warnings about multiple
definitions of global variables. Ignore them; that's just ANSI pedantism.

* On Macintoshes under A/UX with gcc. It may be necessary to add
-D_SYSV_SOURCE to OPTS.



