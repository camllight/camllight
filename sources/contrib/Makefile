### Configuration Makefile to build the Caml Light packages.

# The default configuration set up in this Makefile should work as is for
# Linux or Windows under Cygwin.
# The architecture is supposed to be pentium.

# Notes:
# - the configuration has been tested on a Debian distribution, various BSD
#   and some more exotic Unixes.
# - many other OSes are fully supported including the BSDs: for those Oses,
#   you must change the default using the relevant settings that are commented out in the
#   corresponding entry for Linux.
# - in case something is not found (a C include or a C library), even with
#   the suggested setting values, you should try to find the missing item
#   using the Unix locate command; then add the corresponding path to the
#   corresponding Makefile section entry.

#(0) Which packages to compile and install.

# See the file INDEX for a description of the packages and their requirements.
# Remember that "libunix" is required for
# "debugger", "libgraph", "camltk", "camltk4", and "search_isos".
PACKAGES=libunix libgraph debugger libnum libstr mletags \
  camlmode lorder profiler camltk4 camlsearch caml-tex

# By default caml-latex2e is not installed:
# you have to choose between caml-tex and caml-latex2e

#(1) Which C compiler to use with which flags.

# Should be the same compiler used to compile the main system (../src).
CC=gcc

# Any additional options to $(CC).
OPTS=

# How to call the C preprocessor on a file that does not have the .c extension.
# That's /lib/cpp on most machines, sometimes /usr/bin/cpp,
# and /usr/ccs/lib/cpp under Solaris.
# The -P option suppresses the generation of "# linenum" directives,
# which are not understood by Caml Light.
# For Linux, generally
#CPP=/lib/cpp -P
# For *BSD, generally
CPP=/usr/bin/cpp -P

# Test to see whether ranlib exists on the machine
RANLIBTEST=test -f /usr/bin/ranlib -o -f /bin/ranlib

# How to invoke ranlib
RANLIB=ranlib

#(2) Where to install the compiled objects.

# The directory where public executables will be installed
BINDIR=/usr/local/bin

# The directory where library files will be installed
LIBDIR=/usr/local/lib/caml-light

# The manual section where the manual pages will be installed
MANEXT=1

# The directory where the manual pages will be installed
MANDIR=/usr/local/man/man$(MANEXT)

#(3) Where to find C includes and libraries for the packages.

#(3.1) Needed for the "libgraph" and "camltk" packages: path to X.h.

# The path to the include directory containing the X11/*.h includes,
# for instance libX11.h.
# (usually /usr/include; for SunOS with OpenLook, /usr/openwin/include)
# For True64 Unix
#X11_INCLUDES=/usr/include/X11
# For Linux, generally
X11_INCLUDES=/usr/X11R6/include/X11
# For *BSD, should be
#X11_INCLUDES=/usr/local/include/X11
# For *BSD or Linux, could also be
#X11_INCLUDES=/usr/X11R6/include/

#(3.2) Needed for the "libgraph" and "camltk" packages: path to libX11.a.

# The path to the directory containing the X11 libraries, for instance
# libX11.a (usually /usr/lib; for SunOS with OpenLook, /usr/openwin/lib)
# For True64 Unix
#X11_LIB=/usr/lib/X11
# For Linux, generally
X11_LIB=/usr/X11R6/lib
# For *BSD, should be
#X11_INCLUDES=/usr/local/lib/X11
# For *BSD or Linux, could also be
#X11_INCLUDES=/usr/X11R6/lib/

#(3.3) Needed for the libnum library (arbitrary-precision arithmetic).

# Name of the target architecture.
# Used only for the libnum library (arbitrary-precision arithmetic), to
# select assembly-language versions of time-critical low-level routines.
# Known targets:
#       68K     vax     ns      mips    alpha   pyramid i960
#       sparc   supersparc      sparc-solaris   supersparc-solaris
#       pentium
# See the file libnum/README for more explanations.
# If you don't know, leave BIGNUM_ARCH=C, which selects a portable
# C implementation of these routines.
# For MacOSX:
BIGNUM_ARCH=C
#BIGNUM_ARCH=pentium

#(3.4) Needed only for the "camltk" package.

#(3.4.1) Name of the directories where the tcl.h and tk.h includes can be found.
# For True64 Unix
#TCLINCLDIR=/usr/local/lib/tcl8.0/include
#TKINCLDIR=/usr/local/lib/tk8.0/include
# For Linux, generally
TCLINCLDIR=/usr/include
TKINCLDIR=/usr/include
# For *BSD
#TCLINCLDIR=/usr/local/include/tcl8.4
#TKINCLDIR=/usr/local/include/tk8.4

#(3.4.2) Name of the directories where the libtcl.a and libtk.a libraries can be found
# Needed only for the "camltk" package.
# /usr/local configuration
# For True64 Unix
#TKLIBDIR=/usr/local/lib/tk8.0/lib
#TCLLIBDIR=/usr/local/lib/tcl8.0/lib
# For Linux, generally
#TKLIBDIR=/usr/lib
#TCLLIBDIR=/usr/lib
# For *BSD, generally
TKLIBDIR=/usr/local/lib
TCLLIBDIR=/usr/local/lib

#(3.4.3) List of libraries required to link Tcl/Tk. This may help if you have
# a custom Tcl/Tk with various extensions (such as Xpm)
# For True64 Unix
#TKLIBS=-ltk8.0 -ltcl8.0 -lX11 -lm
# For Linux generally
#TKLIBS=-ltk84 -ltcl84 -lX11 -ldl -lm
# Could also be
#TKLIBS=-ltk -ltcl -lX11
#TKLIBS=-ltk8.4 -ltcl8.4 -lX11 -ldl -lm
#TKLIBS=-ltk8.5 -ltcl8.5 -lX11 -ldl -lm
# For *BSD
#TKLIBS=-ltk84 -ltcl84 -lX11
TKLIBS=-ltk85 -ltcl85 -lX11

#(4) Where to install the text files for LaTeX and emacs.

#(4.1) Name of the directory where LaTeX style files should be installed.

# Should be a sub-directory of the directory Latex, in which you find
# a lot of auxilliary styles (.sty files).
# Needed only for the "caml-tex" and "caml-latex2e" packages.
# For True64 Unix
#TEXINPUTDIR=/usr/local/teTeX/share/texmf/tex/latex/misc
# For Linux, generally
TEXINPUTDIR=/usr/share/texmf/tex/latex/misc

#(4.2) Name of the directory where Emacs Lisp files should be installed.

# Needed only for the "camlmode" package. The location of the
# site-lisp sub-directory.
# For True64 Unix
#EMACSLISPDIR=/usr/local/lib/emacs/site-lisp
# For Linux, generally
EMACSLISPDIR=/usr/share/emacs/site-lisp

#(5) Makefile trickery, in case make does not use the right shell flavor.

# Some "make"s need this to ensure that they call /bin/sh, not /bin/csh
# Seems harmless on most other "make"s. Try removing this line if you
# run into trouble.
SHELL=/bin/sh

### End of configuration section

SUBDIRS=libunix libgraph debugger libnum libstr mletags \
  caml-tex camlmode lorder profiler camltk4 camlsearch

DEFINES=\
  CC='$(CC)' \
  OPTS='$(OPTS)' \
  CPP='$(CPP)' \
  RANLIBTEST='$(RANLIBTEST)' \
  RANLIB='$(RANLIB)' \
  BINDIR='$(BINDIR)' \
  LIBDIR='$(LIBDIR)' \
  MANDIR='$(MANDIR)' \
  MANEXT='$(MANEXT)' \
  X11_INCLUDES='$(X11_INCLUDES)' \
  X11_LIB='$(X11_LIB)' \
  BIGNUM_ARCH='$(BIGNUM_ARCH)' \
  TEXINPUTDIR='$(TEXINPUTDIR)' \
  EMACSLISPDIR='$(EMACSLISPDIR)' \
  TCLINCLDIR='$(TCLINCLDIR)' \
  TKINCLDIR='$(TKINCLDIR)' \
  TKLIBDIR='$(TKLIBDIR)' \
  TCLLIBDIR='$(TCLLIBDIR)' \
  TKLIBS='$(TKLIBS)'


# Compile everything
all:
	@set -e; set -x; for d in $(PACKAGES); do (cd $$d; $(MAKE) $(DEFINES) all); done

# Install everything
install:
	@set +e; set -x; for d in $(PACKAGES); do (cd $$d; $(MAKE) $(DEFINES) install); done

# Remove all generated files
clean:
	@set +e; set -x; for d in $(SUBDIRS); do (cd $$d; $(MAKE) $(DEFINES) clean); done

# Rebuild the dependencies
depend:
	@set +e; set -x; for d in $(SUBDIRS); do (cd $$d; $(MAKE) $(DEFINES) depend); done

