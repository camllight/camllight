### Configuration section

# Which packages to compile and install.
# See the file INDEX for a description of the packages and their requirements.
# Remember that "libunix" is required for
# "debugger", "libgraph", "camltk",  and "search_isos".
PACKAGES=libunix libgraph debugger libnum libstr mletags \
  caml-tex camlmode lorder profiler camltk camlsearch

# Which C compiler to use.
# Should be the same compiler used to compile the main system (../src).
CC=cc

# Any additional options to $(CC).
OPTS=

# How to call the C preprocessor on a file that does not have the .c extension.
# That's /lib/cpp on most machines, sometimes /usr/bin/cpp,
# and /usr/ccs/lib/cpp under Solaris.
# The -P option suppresses the generation of "# linenum" directives,
# which are not understood by Caml Light.
CPP=/lib/cpp -P

# Test to see whether ranlib exists on the machine
RANLIBTEST=test -f /usr/bin/ranlib -o -f /bin/ranlib

# How to invoke ranlib
RANLIB=ranlib

# The directory where public executables will be installed
BINDIR=/usr/local/bin

# The directory where library files will be installed
LIBDIR=/usr/local/lib/caml-light

# The manual section where the manual pages will be installed
MANEXT=1

# The directory where the manual pages will be installed
MANDIR=/usr/local/man/man$(MANEXT)

# The path to the include directory containing the X11/*.h includes
# (usually /usr/include; for SunOS with OpenLook, /usr/openwin/include)
# Needed only for the "libgraph" package.
X11_INCLUDES=/usr/include

# The path to the directory containing the X11 libraries.
# (usually /usr/lib; for SunOS with OpenLook, /usr/openwin/lib)
# Needed for the "libgraph" and "camltk" packages.
X11_LIB=/usr/lib

# Name of the target architecture.
# Used only for the libnum library (arbitrary-precision arithmetic), to
# select assembly-language versions of time-critical low-level routines.
# Known targets: 68K, vax, ns, mips, alpha, pyramid, i960, sparc, supersparc.
# See the file libnum/README for more explanations.
# If you don't know, leave BIGNUM_ARCH=C, which selects a portable
# C implementation of these routines.
BIGNUM_ARCH=C

# Name of the directory where TeX input files should be installed.
# Needed only for the "caml-tex" package.
TEXINPUTDIR=/usr/local/tex/inputs

# Name of the directory where Emacs Lisp files should be installed.
# Needed only for the "camlmode" package.
EMACSLISPDIR=/usr/local/emacs/site-lisp

# Name of the directories where the tcl.h and tk.h includes can be found.
# Needed only for the "camltk" package.
TCLINCLDIR=/usr/local/lib/tcl/include
TKINCLDIR=/usr/local/lib/tk/include

# Name of the directories where the libtcl.a and libtk.a libraries can be found
# Needed only for the "camltk" package.
TKLIBDIR=/usr/local/lib/tk/lib
TCLLIBDIR=/usr/local/lib/tcl/lib

# List of libraries required to link Tcl/Tk. This may help if you have
# a custom Tcl/Tk with various extensions (such as Xpm)
TKLIBS=-ltk -ltcl -lX11

# Some "make"s need this to ensure that they call /bin/sh, not /bin/csh
# Seems harmless on most other "make"s. Try removing this line if you
# run into trouble.
SHELL=/bin/sh

### End of configuration section

SUBDIRS=libunix libgraph debugger libnum libstr mletags \
  caml-tex camlmode lorder profiler camltk camlsearch

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
